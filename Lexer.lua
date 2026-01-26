--!optimize 2
--!native

export type TokenKind = "BrokenUnicode" | "Character" | "Number" |
"QuotedString" | "BrokenString" | "Comment" |
"BlockComment" | "BrokenComment" | "RawString" |
"BrokenString" | "Attribute" | "SubAssign" |
"SkinnyArrow" | "DivAssign" | "FloorDiv" |
"DoubleColon" | "ConcatAssign" |
"Dot3" | "Dot2" | "AddAssign" |
"MulAssign" | "ModAssign" | "PowAssign" |
"LessEqual" | "GreaterEqual" | "NotEqual" |
"Equal" | "BrokenString" | "BrokenInterpDoubleBrace" |
"InterpStringBegin" | "InterpStringSimple" | "InterpStringMid"

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

-- most of these byte comparison functions should be inlined

--- Compares byte to assignable bytes
--- Character set: [+*%^<>~=]
--- Byte set: [\43\42\37\94\60\62\126\61]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is an assignable byte
local function IsAssignable(byte : number) : boolean -- basically just for character= assignable stuff
	return byte == 43 -- +
		or byte == 	42 -- *
		or byte == 37 -- %
		or byte == 94 -- ^
		or byte == 60 -- <
		or byte == 62 -- >
		or byte == 126 -- ~
		or byte == 61 -- =
	-- or byte == 46 -- . / not needed here since it is handled elsewhere
end

--- Compares byte to lexerable bytes
--- Character set: [#(){];,&|?]
--- Byte set: [\35\40\41\123\125\93\59\44\38\124\63]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is a lexerable byte
local function IsLexerable(byte : number) -- should only contain lexable bytes that ARE NOT ALREADY CHECKED
	return byte == 35 -- #
		or byte == 40 -- (
		or byte == 41 -- )
		or byte == 123 -- {
		--or byte == 125 -- } / not needed here since it is handled in the main loop
		or byte == 93 -- ]
		or byte == 59 -- ;
		or byte == 44 -- ,
		or byte == 38 -- &
		or byte == 124 -- |
		or byte == 63 -- ?
end

--- Compares byte to lowercase alphabetical bytes
--- Character set: [a-z]
--- Byte set: [\97-\122]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is a lowercase alphabetical byte
local function IsLowerAlphabetical(byte : number)
	return byte >= 97 and byte <= 122
end

--- Compares byte to digit bytes
--- Character set: [0-9]
--- Byte set: [\48-57]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is a digit byte
local function IsDigit(byte : number)
	return byte >= 48 and byte <= 57
end

--- Compares byte to alphabetical bytes
--- Character set: [a-Z]
--- Byte set: [\97-122\65-\90]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is an alphabetical byte
local function IsAlphabetical(byte : number)
	return IsLowerAlphabetical(byte) or (byte >= 65 and byte <= 90)
end

--- Compares byte to alphanumerical bytes
--- Character set: [0-9] & [a-Z]
--- Byte set: [\48-\57\97-122\65-\90]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is an alphanumerical byte
local function IsAlphanumeric(byte : number)
	return IsDigit(byte) or IsAlphabetical(byte)
end

--- Compares byte to alphabetical identifier bytes
--- Character set: [a-Z] & _
--- Byte set: [\97-122\65-\90\95]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is an alphabetical identifier byte
local function IsAlphabeticalIdentifier(byte : number)
	return IsAlphabetical(byte) or byte == 95
end

--- Compares byte to alphaNumerical identifier bytes
--- Character set: [0-9] & [a-Z] & _
--- Byte set: [\48-\57\97-122\65-\90\95]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is an identifier byte
local function IsAlphanumericIdentifier(byte : number)
	return IsAlphanumeric(byte) or byte == 95
end

--- Compares byte to newline bytes
--- Character set: [\n\r]
--- Byte set: [\10\13]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is a newline byte
local function IsNewLine(byte : number)
	return byte == 10 or byte == 13 -- \n \r
end

--- Compares byte to whitespace bytes
--- Character set: [\t\n\v\f\r ]
--- Byte set: [\9-\13\32]
---
--- @param byte number The byte you are comparing
--- @return boolean Returns a boolean corisponding to if the provided byte is a whitespace byte
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

return function(source : buffer)
	local tokens = {}
	local tokenCount = 0

	local sourceSize = buffer.len(source)
	local base0Size = sourceSize - 1
	
	local furthestPosition = -1
	local stringInterpDepth = 0
	
	local currentPosition = 0
	local currentLine = 1
	local steppedLines = 0

	local function ConsumeBytes(Amount : number)
		currentPosition += Amount
	end

	local function ConsumeLines()
		if steppedLines ~= 0 then
			currentLine += steppedLines
			steppedLines = 0
		end
	end

	local function AppendToken<t>(TokenData : t)
		tokenCount += 1
		tokens[tokenCount] = TokenData
	end

	local function CreateTokenInfo(length : number, startLine : number, endLine : number)
		return {
			Start = CreatePositionInfo(currentPosition, startLine),
			End = CreatePositionInfo(currentPosition + length, endLine)
		}
	end

	local function addToken(tokenLength : number, tokenType : string, startLine : number, shouldConsume : boolean)
		AppendToken({
			Value = buffer.readstring(source, currentPosition, tokenLength),
			Kind = tokenType,
			Info = CreateTokenInfo(tokenLength, startLine, currentLine + steppedLines)
		})

		if shouldConsume then
			ConsumeBytes(tokenLength)
		end
	end

	local function addUnicodeToken(unicodeLength : number, startline : number, codepoint : number?)
		AppendToken({
			Value = buffer.readstring(source, currentPosition, unicodeLength),
			Kind = "BrokenUnicode",
			Info = CreateTokenInfo(unicodeLength, startline, startline),
			Codepoint = codepoint
		})

		ConsumeBytes(unicodeLength)
	end

	local function addSameLineToken(tokenLength : number, tokenType : string, startLine : number)
		AppendToken({
			Value = buffer.readstring(source, currentPosition, tokenLength),
			Kind = tokenType,
			Info = CreateTokenInfo(tokenLength, startLine, startLine)
		})

		ConsumeBytes(tokenLength)
	end

	local function addByte()
		addToken(1, "Character", currentLine, true)
	end

	local function canPeek(Amount : number)
		return currentPosition + Amount <= base0Size
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
		return if canPeek(Amount or 0) then
			rawPeek(Amount or 0)
		else
			nil -- return nil here just to make it an iterator
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

			offset += if signByte and (signByte == 43 or signByte == 45) then -- + -
					2
				else
					1
		end

		while canPeek(offset) do
			local byte = rawPeek(offset)

			if not IsAlphanumericIdentifier(byte) then -- A-z 0-9 _
				break
			end

			offset += 1
		end

		return offset, "Number"
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

				if byte ~= 61 then
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
				return false, if byte == 93 and canPeek(i + EqualsAmount + 1) then
						i-1 -- recheck the byte as I cannot recursively check as a certain character case would cause a stack overflow which would cause this to have a stack overflow
					else
						i
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

				if byte == 93 then
					if canPeek(1 + offset + Equals) then -- check if the rest of the string is possible to close (ie ===])
						local ValidClose, CheckedAmount = getClosingEquals(offset+1, Equals)

						if ValidClose then
							return true, CheckedAmount + 1
						end
						offset = CheckedAmount
					else
						return false, base0Size - currentPosition
					end
				else
					offset += 1
				end
			end

			return false, offset
		end

		return nil, Equals -- nil for comments
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
				if ismultilinedlongbyte == 91 then
					local IsValid, Length = getMultiLineLength(4)

					return Length, if IsValid then
							"BlockComment"
						else
							"BrokenComment"
				end
				local IsValid, Length = getLongMultiLength(3)

				if IsValid ~= nil then
					return Length, if IsValid then
							"BlockComment"
						else
							"BrokenComment"
				end
				offset = Length + 2 -- basically --[==invalid[ turns it into a single line Comment
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
		if firstbyte == 61 then
			local IsValid, Equals = getLongMultiLength(1)

			return Equals, if IsValid then
					"RawString"
				else
					"BrokenString"
		end
		local IsValid, Length = getMultiLineLength(1)

		return Length, if IsValid then
				"RawString"
			else
				"BrokenString"
	end

	local function getWordLength(compiledword : number)
		local offset = 1

		while canPeek(offset) do
			local byte = rawPeek(offset)

			if not IsAlphanumericIdentifier(byte) then
				break
			end

			offset += 1
		end

		return offset, compiledReservedWords[buffer.readstring(source, currentPosition, offset)] or "Name" -- its overall better to just do one readstring request as it only creates 1 new string instead of a million different strings for each concat, additionally its an unknown for how long someone may name a variable
	end

	local function getAttributeLength() -- wow this code looks farmilier
		local offset = 1

		while canPeek(offset) do
			local byte = rawPeek(offset)

			if not IsAlphanumericIdentifier(byte) then
				break
			end

			offset += 1
		end

		return offset, "Attribute"
	end

	local function getSubtractionLength(nextbyte : number?)
		if nextbyte == 61 or nextbyte == 62 then
			return 2, if nextbyte == 61 then
					"SubAssign"
				else
					"SkinnyArrow"
		end
		return 1, "Character"
	end

	local function getDivisionLength()
		if peek(1) == 47 then
			if peek(2) == 47 then
				return 3, "DivAssign"
			end
			
			return 2, "FloorDiv"
		end

		return 1, "Character"
	end

	local function getColonLength()
		if peek(1) == 58 then
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
					return 3, if thirdByte == 47 then
							"ConcatAssign"
						else
							"Dot3"
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
			return 2, if byte == 43 then
					"AddAssign"
				elseif byte == 42 then
					"MulAssign"
				elseif byte == 37 then
					"ModAssign"
				elseif byte == 94 then
					"PowAssign"
				elseif byte == 60 then
					"LessEqual"
				elseif byte == 62 then
					"GreaterEqual"
				elseif byte == 126 then
					"NotEqual"
				else
					"Equal"
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
				lastescape = if lastescape ~= offset - 1 then offset else nil
				offset += if peek(offset + 1) == 117 and peek(offset + 2) == 123 then
						3 -- offsetting for \u{
					else
						readBackSlash(offset)
			elseif byte == 123 and lastescape ~= offset - 1 then -- {
				stringInterpDepth += 1
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
		if IsAlphabeticalIdentifier(byte) then -- words (a-Z_)
			ConsumeLines()
			local Length, TokenType = getWordLength(byte)
			addSameLineToken(Length, TokenType, currentLine)
		elseif IsDigit(byte) then -- numbers (0-9)
			ConsumeLines()
			local Length, TokenType = getNumberLength()
			addSameLineToken(Length, TokenType, currentLine)
		elseif IsAssignable(byte) then -- assignable (+*%^<>~=)
			ConsumeLines()
			local Length, TokenType = getAssignmentLength(byte)
			addSameLineToken(Length, TokenType, currentLine)
		elseif byte == 91 then -- multilined string ([[...]] / [=[...]=])
			ConsumeLines()
			local nextbyte = peek(1)

			if nextbyte == 91 or nextbyte == 61 then
				local Length, TokenType = getMultiLinedString(nextbyte)
				addToken(Length, TokenType, currentLine, true)
			else
				addByte()
			end
		elseif byte == 34 or byte == 39 then -- double quote (") / single quote (')
			ConsumeLines()
			local Length, TokenType = getNormalStringLength(byte)
			addSameLineToken(Length, TokenType, currentLine)
		elseif byte == 96 then -- string interpolation (`)
			ConsumeLines()

			local Length, TokenType = readInterpolatedStringSection("InterpStringBegin", "InterpStringSimple")
			addToken(Length, TokenType, currentLine, true)
		elseif byte == 45 then -- minus (-)
			ConsumeLines()
			local nextbyte = peek(1)

			if nextbyte then
				if nextbyte == 45 then -- comment (--)
					local Length, TokenType = getCommentLength()
					addToken(Length, TokenType, currentLine, true)
				else
					local Length, TokenType = getSubtractionLength(nextbyte)
					addToken(Length, TokenType, currentLine, true)
				end
			else
				addByte()
			end
		elseif byte == 47 then -- division (/) / (//) / (//=)
			ConsumeLines()
			local Length, TokenType = getDivisionLength()
			addToken(Length, TokenType, currentLine, true)
		elseif byte == 46 then -- dots (.) / concat (..) / vargs (...)
			ConsumeLines()
			local Length, TokenType = getDotLength()
			addToken(Length, TokenType, currentLine, true)
		elseif byte == 58 then -- colons (:) / (::)
			ConsumeLines()
			local Length, TokenType = getColonLength()
			addToken(Length, TokenType, currentLine, true)
		elseif byte == 64 then -- Attributes @
			ConsumeLines()
			local Length, TokenType = getAttributeLength()
			addToken(Length, TokenType, currentLine, true)
		elseif byte == 125 then -- closed bracket (})
			ConsumeLines()

			if stringInterpDepth ~= 0 then
				local Length, TokenType = readInterpolatedStringSection("InterpStringMid", "InterpStringEnd")
				addToken(Length, TokenType, currentLine, true)
				stringInterpDepth -= 1
			else
				addByte()
			end
		elseif IsWhitespace(byte) then
			ConsumeBytes(1)
		elseif not IsLexerable(byte) and byte >= 128 then
			ConsumeLines()
			local Length, Codepoint = readUtf8Error(byte)
			addUnicodeToken(Length, currentLine, Codepoint)
		else
			ConsumeLines()
			addByte()
		end
	end
	ConsumeLines()
	addToken(0, "Eof", currentLine, false)

	return tokens
end
