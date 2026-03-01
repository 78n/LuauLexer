--!optimize 2
--!native

-- Written by https://github.com/78n
-- Ported to Luau https://github.com/luau-lang/luau/blob/master/Ast/src/Lexer.cpp

export type TokenKind = "Number" | "Character" |
"QuotedString" | "RawString" | "BrokenString" |
"InterpStringBegin" | "InterpStringMid" | "InterpStringSimple" | "BrokenInterpDoubleBrace" |
"Comment" | "BlockComment" | "BrokenComment" |
"AddAssign" | "SubAssign" | "MulAssign" | "DivAssign" | "FloorDiv" | "ModAssign" | "PowAssign" | "ConcatAssign" |
"Equal" | "NotEqual" | "LessEqual" | "GreaterEqual" |
"Dot2" | "Dot3" |
"DoubleColon" | "SkinnyArrow" | "Attribute" |
"BrokenUnicode" | "Eof"

export type TokenInfo = {
	Line : number,
	Column : number,
	Offset : number
}

export type TokenLocationInfo = {
	Start : TokenInfo,
	End : TokenInfo
}

export type Token = {
	Info : TokenLocationInfo,
	Value : string,
	Kind : TokenKind,
	Codepoint : number?
}

export type Tokens = {Token}

--- Character set: [+*%^<>~=]
--- Byte set: [\43\42\37\94\60\62\126\61]
local function IsAssignable(byte : number) : boolean -- basically just for character= assignable stuff
	return byte == 43 -- +
		or byte == 	42 -- *
		or byte == 37 -- %
		or byte == 94 -- ^
		or byte == 60 -- <
		or byte == 62 -- >
		or byte == 126 -- ~
		or byte == 61 -- =
		--or byte == 46 -- . / not needed here since it is handled in getDotLength
end

--- Character set: [a-z]
--- Byte set: [\97-\122]
local function IsLowerAlphabetical(byte : number)
	return byte >= 97 and byte <= 122
end

--- Character set: [0-9]
--- Byte set: [\48-57]
local function IsDigit(byte : number)
	return byte >= 48 and byte <= 57
end

--- Character set: [a-Z]
--- Byte set: [\97-122\65-\90]
local function IsAlphabetical(byte : number)
	return IsLowerAlphabetical(byte) or (byte >= 65 and byte <= 90)
end

--- Character set: [0-9] & [a-Z]
--- Byte set: [\48-\57\97-122\65-\90]
local function IsAlphanumeric(byte : number)
	return IsDigit(byte) or IsAlphabetical(byte)
end

--- Character set: [a-Z] & _
--- Byte set: [\97-122\65-\90\95]
local function IsAlphabeticalIdentifier(byte : number)
	return IsAlphabetical(byte) or byte == 95
end

--- Character set: [0-9] & [a-Z] & _
--- Byte set: [\48-\57\97-122\65-\90\95]
local function IsAlphanumericIdentifier(byte : number)
	return IsAlphanumeric(byte) or byte == 95
end

--- Character set: [\n\r]
--- Byte set: [\10\13]
local function IsNewLine(byte : number)
	return byte == 10 or byte == 13 -- \n \r
end

--- Character set: [\t\n\v\f\r ]
--- Byte set: [\9-\13\32]
local function IsWhitespace(byte : number)
	return byte >= 9 and byte <= 13 or byte == 32
end

local function CreatePositionInfo(base : number, line : number)
	return {
		Line = line,
		Column = base + 1, -- convert to base 1
		Offset = base -- base 0
	}
end

local compiledReservedWords = {} do
	local reservedWords = {
		"and", "break", "do", "else", "elseif", "end", "false",
		"for", "function", "if", "in", "local", "nil", "not",
		"or", "repeat", "return", "then", "true", "until", "while"
	}

	for i,v in reservedWords do
		if #v ~= 0 then
			local firstbyte = string.byte(v, 1)

			if IsLowerAlphabetical(firstbyte) then
				firstbyte -= 32
			end

			compiledReservedWords[v] = "Reserved"..string.char(firstbyte)..string.sub(v, 2)
		end
	end
end

return function(source : buffer) : Tokens
	local sourceSize = buffer.len(source)
	local braceStack = {} -- true if the brace is in an interpoled string, false if its in an escape
	local tokens = {}
	
	local furthestPosition = -1
	local currentPosition = 0

	local steppedLines = 0
	local currentLine = 1

	local function consume(Amount : number)
		currentPosition += Amount
	end

	local function consumeLines()
		if steppedLines ~= 0 then
			currentLine += steppedLines
			steppedLines = 0
		end
	end

	local function appendToken(TokenData : Token)
		table.insert(tokens, TokenData)
	end

	local function createTokenInfo(length : number, startLine : number, endLine : number)
		return {
			Start = CreatePositionInfo(currentPosition, startLine),
			End = CreatePositionInfo(currentPosition + length, endLine)
		}
	end

	local function addToken(tokenLength : number, tokenType : string, startLine : number, dontconsume : boolean?)
		appendToken({
			Value = buffer.readstring(source, currentPosition, tokenLength),
			Kind = tokenType,
			Info = createTokenInfo(tokenLength, startLine, currentLine + steppedLines)
		})

		if not dontconsume then
			consume(tokenLength)
		end
	end

	local function addUnicodeToken(unicodeLength : number, startline : number, codepoint : number?)
		appendToken({
			Value = buffer.readstring(source, currentPosition, unicodeLength),
			Kind = "BrokenUnicode",
			Info = createTokenInfo(unicodeLength, startline, startline),
			Codepoint = codepoint
		})

		consume(unicodeLength)
	end

	local function addSameLineToken(tokenLength : number, tokenType : string, startLine : number)
		appendToken({
			Value = buffer.readstring(source, currentPosition, tokenLength),
			Kind = tokenType,
			Info = createTokenInfo(tokenLength, startLine, startLine)
		})

		consume(tokenLength)
	end

	local function addByte()
		addToken(1, "Character", currentLine)
	end

	local function canPeek(Amount : number)
		return currentPosition + Amount < sourceSize
	end

	local function rawPeek(Amount : number)
		local byte = buffer.readu8(source, currentPosition + Amount)

		if IsNewLine(byte) and furthestPosition < currentPosition + Amount then
			steppedLines += 1
			furthestPosition = currentPosition + Amount
		end

		return byte
	end

	local function peek(Amount : number?)
		return if canPeek(Amount or 0) then rawPeek(Amount or 0) else nil -- nil here to make it an iterator
	end

	local function readName(offset)
		while canPeek(offset) do
			local byte = rawPeek(offset)

			if not IsAlphanumericIdentifier(byte) then
				break
			end

			offset += 1
		end
		
		return offset
	end

	local function getNumberLength()
		local offset = 1

		while canPeek(offset) do
			local byte = rawPeek(offset)

			if not (IsDigit(byte) or byte == 46 or byte == 95) then -- 0-9 . _
				break
			end

			offset += 1
		end

		local expByte = peek(offset)

		if expByte and (expByte == 101 or expByte == 69) then -- E/e
			local signByte = peek(offset + 1)

			offset += if signByte and (signByte == 43 or signByte == 45) then 2 else 1 -- +-
		end

		return readName(offset), "Number"
	end

	local function readBackSlash(offset : number)
		local escByte = peek(offset + 1)

		if escByte then
			if escByte == 13 then -- \r
				if peek(offset + 2) == 10 then -- \n
					return 2
				end
			elseif escByte == 122 then -- \z
				local offsetAmount = 2

				while canPeek(offset + offsetAmount) and IsWhitespace(rawPeek(offset + offsetAmount)) do
					offsetAmount += 1
				end

				return offsetAmount
			end
		end

		return 1
	end

	local function getNormalStringLength(strbyte: number)
		local lastescape
		local offset = 1

		while canPeek(offset) do
			local byte = rawPeek(offset)

			if IsNewLine(byte) then
				break
			elseif byte == 92 then -- \ escape
				lastescape = if lastescape ~= offset - 1 then offset else nil
				offset += readBackSlash(offset)
			elseif byte == strbyte and lastescape ~= offset - 1 then
				return offset+1, "QuotedString"
			else
				offset += 1
			end
		end

		return offset, "BrokenString"
	end

	local function getEqualsCount(PositionOffset : number) -- returns a tuple (first being a boolean with the success status (if its a valid long multiline) and the second parameter being how many equals there are) This function should only be used to retrieve the equals amount for long multilines (ie: [==[) and stuff
		local offset = 1 -- when calling this function there should have already been a verification that this method is needed (ie the [= )

		while canPeek(PositionOffset + offset) do
			local byte = rawPeek(PositionOffset + offset)

			if byte == 91 then -- [
				return true, offset
			else
				if not IsNewLine(byte) then
					offset += 1
				end

				if byte ~= 61 then -- =
					break
				end
			end
		end

		return false, offset
	end

	local function getClosingEquals(PositionOffset : number, EqualsAmount : number)
		for i = PositionOffset, PositionOffset + EqualsAmount-1 do
			local byte = rawPeek(i)

			if byte ~= 61 then -- does not equal an =
				return false, i
			end
		end

		return rawPeek(PositionOffset + EqualsAmount) == 93, PositionOffset + EqualsAmount
	end

	local function getLongMultiLength(baseoffset : number)
		local IsValid, Equals = getEqualsCount(baseoffset)

		if IsValid then
			local offset = baseoffset + 1

			while canPeek(offset) do
				local byte = rawPeek(offset)

				if byte == 93 then -- ]
					if canPeek(1 + offset + Equals) then -- check if the rest of the string is possible to close (ie ===])
						local ValidClose, CheckedAmount = getClosingEquals(offset+1, Equals)

						if ValidClose then
							return true, CheckedAmount + 1
						end
						offset = CheckedAmount
					else
						return false, sourceSize - currentPosition
					end
				else
					offset += 1
				end
			end
			return false, offset
		end
		return nil, Equals+1 -- nil for comments
	end

	local function getMultiLineLength(baseoffset : number)
		while canPeek(baseoffset) do
			local byte = rawPeek(baseoffset)

			if byte == 93 then -- ]
				local nextbyte = peek(baseoffset + 1)

				if not nextbyte then
					baseoffset += 1
					break 
				end

				baseoffset += 2

				if nextbyte == 93 then -- ]
					return true, baseoffset -- is closed
				end
			else
				baseoffset += 1
			end
		end
		return false, baseoffset -- isnt closed
	end

	local function getCommentLength()
		local ismultilinebyte = peek(2)

		if not ismultilinebyte or IsNewLine(ismultilinebyte) then
			return 2, "Comment"
		end

		local offset

		if ismultilinebyte == 91 then -- multiline comment check [
			local ismultilinedlongbyte = peek(3)

			if not ismultilinedlongbyte or IsNewLine(ismultilinedlongbyte) then -- checks if it got a valid char or if it its still in the comment
				return 3, "Comment"
			end

			if ismultilinedlongbyte == 91 or ismultilinedlongbyte == 61 then
				local tempStepped = steppedLines
				local IsValid, Length

				if ismultilinedlongbyte == 91 then
					IsValid, Length = getMultiLineLength(4)

					return Length, (if IsValid then "BlockComment" else "BrokenComment"), tempStepped ~= steppedLines
				end
				IsValid, Length = getLongMultiLength(3)

				if IsValid ~= nil then
					return Length, (if IsValid then "BlockComment" else "BrokenComment"), tempStepped ~= steppedLines
				end
				offset = Length + 1 -- basically --[==invalid[ turns it into a single line Comment
			else
				offset = 4 -- no point in rechecking the [\.
			end
		else
			offset = 3 -- no point in rechecking the [
		end

		while canPeek(offset) do
			local byte = rawPeek(offset)

			if IsNewLine(byte) then
				break -- \n and \r end the single line comment
			end

			offset += 1
		end
		return offset, "Comment" -- 2 to account for the comment (--)
	end

	local function getMultiLinedString(firstbyte : number)
		local IsValid, Length = (if firstbyte == 61  then getLongMultiLength else getMultiLineLength)(1)

		return Length, (if IsValid then "RawString" else "BrokenString")
	end

	local function getWordLength(compiledword : number)
		local offset = readName(1)

		return offset, compiledReservedWords[buffer.readstring(source, currentPosition, offset)] or "Name"
	end

	local function getAttributeLength()
		return readName(1), "Attribute"
	end

	local function getSubtractionLength(nextbyte : number?)
		if nextbyte == 61 or nextbyte == 62 then -- =>
			return 2, (if nextbyte == 61 then "SubAssign" else "SkinnyArrow")
		end
		return 1, "Character"
	end

	local function getDivisionLength()
		if peek(1) == 47 then -- /
			if peek(2) == 47 then -- /
				return 3, "DivAssign"
			end
			return 2, "FloorDiv"
		end
		return 1, "Character"
	end

	local function getColonLength()
		if peek(1) == 58 then -- :
			return 2, "DoubleColon"
		end
		return 1, "Character"
	end

	local function getDotLength()
		local secondbyte = peek(1)

		if secondbyte then
			if secondbyte == 46 then
				local thirdByte = peek(2)

				if thirdByte and (thirdByte == 47 or thirdByte == 46) then
					return 3, (if thirdByte == 47 then "ConcatAssign" else "Dot3")
				end
				return 2, "Dot2"
			elseif IsDigit(secondbyte) then
				return getNumberLength()
			end
		end
		return 1, "Character"
	end

	local function getAssignmentLength(byte : number)
		if peek(1) == 61 then
			return 2, (if byte == 43 then "AddAssign" elseif byte == 42 then "MulAssign" elseif byte == 37 then "ModAssign" elseif byte == 94 then "PowAssign" elseif byte == 60 then "LessEqual" elseif byte == 62 then "GreaterEqual" elseif byte == 126 then "NotEqual" else "Equal")
		end
		return 1, "Character"
	end

	local function readInterpolatedStringSection(formatType : string, endType : string) --rewrite this poop because i lowkey have NO IDEA wtf i was thinking when originally writing this :sob:
		local lastescape
		local offset = 1

		while canPeek(offset) do
			local byte = rawPeek(offset)

			if byte == 96 and lastescape ~= offset - 1 then -- `
				return offset + 1, endType
			elseif IsNewLine(byte) or byte == 0 then -- \n\r\0
				return offset, "BrokenString"
			elseif byte == 92 then -- \
				lastescape = lastescape ~= offset - 1 and offset
				offset += (if peek(offset + 1) == 117 and peek(offset + 2) == 123 then 3 else readBackSlash(offset)) -- offsetting for \u{
			elseif byte == 123 and lastescape ~= offset - 1 then -- {
				table.insert(braceStack, true)
				if peek(offset + 1) == 123 then
					return offset + 2, "BrokenInterpDoubleBrace"
				end

				return offset + 1, formatType
			else
				offset += 1
			end
		end
		return offset, endType
	end

	local function readUtf8Error(byte : number)
		local codepoint
		local size

		if byte >= 192 and byte <= 223 then -- its faster to compare than do the bitwise AND 224 and compare it to 192
			size = 2
			codepoint = 192
		elseif byte >= 224 and byte <= 239 then -- same reason ^
			size = 3
			codepoint = 224
		elseif byte >= 240 and byte <= 247 then -- ^
			size = 4
			codepoint = 240
		else -- invalid bytes 248 < byte <= 255
			return 1, nil
		end

		for i = 1, size - 1 do
			local nextByte = peek(i)

			if nextByte and bit32.band(nextByte, 192) == 128 then
				codepoint = bit32.bor(bit32.lshift(codepoint, 6), bit32.band(nextByte, 63))
			else
				return i, nil
			end
		end
		return size, codepoint
	end

	for byte in peek do
		if IsWhitespace(byte) then
			consume(1)
		else
			consumeLines()

			if IsAlphabeticalIdentifier(byte) then -- words (a-Z_)
				local Length, TokenType = getWordLength(byte)
				addSameLineToken(Length, TokenType, currentLine)
			elseif IsDigit(byte) then -- numbers (0-9)
				local Length, TokenType = getNumberLength()
				addSameLineToken(Length, TokenType, currentLine)
			elseif IsAssignable(byte) then -- assignable (+*%^<>~=)
				local Length, TokenType = getAssignmentLength(byte)
				addSameLineToken(Length, TokenType, currentLine)
			elseif byte == 91 then -- multilined string ([[...]] / [=[...]=])
				local nextbyte = peek(1)

				if nextbyte == 91 or nextbyte == 61 then
					local Length, TokenType = getMultiLinedString(nextbyte)
					addToken(Length, TokenType, currentLine)
				else
					addByte()
				end
			elseif byte == 34 or byte == 39 then -- double quote (") / single quote (')
				local Length, TokenType = getNormalStringLength(byte)
				addSameLineToken(Length, TokenType, currentLine)
			elseif byte == 96 then -- string interpolation (`)
				local Length, TokenType = readInterpolatedStringSection("InterpStringBegin", "InterpStringSimple")
				addSameLineToken(Length, TokenType, currentLine)
			elseif byte == 45 then -- minus (-)
				local nextbyte = peek(1)

				if nextbyte then
					if nextbyte == 45 then -- comment (--)
						local Length, TokenType, IsMultiLined = getCommentLength();

						(if IsMultiLined then addToken else addSameLineToken)(Length, TokenType, currentLine)
					else -- (-) / (-=) / (->)
						local Length, TokenType = getSubtractionLength(nextbyte)
						addSameLineToken(Length, TokenType, currentLine)
					end
				else
					addByte()
				end
			elseif byte == 47 then -- division (/) / (//) / (//=)
				local Length, TokenType = getDivisionLength()
				addSameLineToken(Length, TokenType, currentLine)
			elseif byte == 46 then -- dots (.) / concat (..) / vargs (...)
				local Length, TokenType = getDotLength()
				addSameLineToken(Length, TokenType, currentLine)
			elseif byte == 58 then -- colons (:) / (::)
				local Length, TokenType = getColonLength()
				addSameLineToken(Length, TokenType, currentLine)
			elseif byte == 64 then -- Attributes @
				local Length, TokenType = getAttributeLength()
				addSameLineToken(Length, TokenType, currentLine)
			elseif byte == 123 then -- open brace ({)
				if braceStack[1] ~= nil then
					table.insert(braceStack, false)
				end
				addByte()
			elseif byte == 125 then -- closed brace (})
				if braceStack[1] ~= nil and table.remove(braceStack) then
					local Length, TokenType = readInterpolatedStringSection("InterpStringMid", "InterpStringEnd")
					addSameLineToken(Length, TokenType, currentLine)
				else
					addByte()
				end
			elseif byte >= 128 then
				local Length, Codepoint = readUtf8Error(byte)
				addUnicodeToken(Length, currentLine, Codepoint)
			else
				addByte()
			end
		end
	end
	consumeLines()
	addSameLineToken(0, "Eof", currentLine)

	return tokens
end
