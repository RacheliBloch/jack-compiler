--RACHEL BLOCH 211889092
--SHIRA BURSTEIN 314741299
include std/pretty.e
include std/filesys.e 
include std/wildcard.e
include std/console.e	
include std/sequence.e  
include std/text.e
include std/convert.e
include std/map.e
include std/math.e 
with trace

object path = "C:\\Euphoria\\euphoria\\Tar5"
constant IN = 0, OUT = 1, FALSE = 0, TRUE = 1, EOF = -1
integer ok
sequence answer, filenameTmp, data, fname
atom counter = 0
sequence keyword = {"class","constructor","function","method","field","static","var","int","char","boolean","void","true","false","null","this","let","do","if","else","while","return"}
sequence symbol = "{}()[].,;+-*/&|<>=~}"
sequence symbolAscii = {'{','}','(',')','[',']','.',',',';','+','-','*','/','&','|','<','>','=','~',' ','\t','"','\n'}
sequence keywordConstant={"true","false","null","this"}
sequence fullpath, xmlpath, Txmlpath, fName
integer fn, fhandle, fn_xml, fn_Txml,fn_data
object line
sequence splitLine, db
integer current

map class_scope_table = map:new() 
map methods_scope_table = map:new()
constant Table_NAME = 1, Table_TYPE = 2, Table_KIND = 3, Table_NUM = 4
sequence className, funcName
integer argCounter = 0, varCounter = 0, fieldCounter = 0, staticCounter = 0, ifCounter = 0, subroutineDecFlag = 0

function printPop(sequence vKind, object vNum)
	printf(fhandle, "pop " & vKind & " ")
	print(fhandle, vNum)
	printf(fhandle, "\n")
	return 0
end function

function printPush(sequence vKind, integer vNum)
	printf(fhandle, "push " & vKind & " ")
	printf(fhandle,"%d", {vNum})
	printf(fhandle, "\n")
	return 0
end function

function classTag(sequence res)
	className = res[2][2]
	clear(class_scope_table)
	fieldCounter = 0
	staticCounter = 0
	ifCounter = 0
	return 0
end function

function classVarDec(sequence res)
	if equal(res[1][2], "static") then
		for i=3 to length(res) by 2 do			
			put(class_scope_table, res[i][2], {res[2][2] ,"static" , staticCounter}) -- name, type, static, counter
			staticCounter = staticCounter + 1
		end for
	else
		for i=3 to length(res) by 2 do			
			put(class_scope_table, res[i][2], {res[2][2] ,"this" , fieldCounter}) -- name, type, field, counter
			fieldCounter = fieldCounter + 1
		end for
	end if
	return 0
end function

function SubroutineDec(sequence res)
	clear(methods_scope_table)
	argCounter = 0
	varCounter = 0
	subroutineDecFlag = 0
	funcName=res[3][2]
	if equal(res[1][2], "method") then
		put(methods_scope_table, "this", {className ,"argument" , argCounter}) -- name, type, argument, counter
		argCounter = argCounter + 1
	end if
	
	sequence funcType = res[1][2]
	switch funcType do
		case "constructor" then
			subroutineDecFlag = 1
		case "function" then
			subroutineDecFlag = 2
		case "method" then
			subroutineDecFlag = 3			
	end switch
	return 0
end function

function PrintSubroutineDec()
	if not(subroutineDecFlag = 0) then
		printf(fhandle, "function " & className & "." & funcName & " %d\n", varCounter)
	end if
	switch subroutineDecFlag do
		case 0 then
		case 1 then
			printPush("constant", fieldCounter)
			printf(fhandle,"call Memory.alloc 1\n")
			printPop("pointer", to_integer(0))
			subroutineDecFlag = 0
		case 2 then
			subroutineDecFlag = 0
		case 3 then
			printPush("argument", 0)
			printPop("pointer", to_integer(0))
			subroutineDecFlag = 0
	end switch
	return 0
end function

function parameterList(sequence res)
	if sequence(res) and equal(res ,{}) then
		-- do nothing
	else
		for i=1 to length(res) by 3 do			
		put(methods_scope_table, res[i+1][2], {res[i][2] ,"argument" , argCounter}) -- name, type, argument, counter
		argCounter = argCounter + 1
	end for
	end if
	return 0
end function

function varDec(sequence res)
	for i=3 to length(res) by 2 do			
		put(methods_scope_table, res[i][2], {res[2][2] ,"local" , varCounter}) -- name, type, var, counter
		varCounter = varCounter + 1
	end for
	return 0
end function

function pushVarName(sequence res)
	object data = get(methods_scope_table, res)	-- this var appears in class_scope_table	
	if atom(data) and data = 0 then	
		data = get(class_scope_table, res)  -- this var appears in methods_scope_table
	end if
	printPush(data[2],data[3])
	return 0
end function

function popVarName(sequence res)
	object data = get(methods_scope_table, res)	-- this var appears in class_scope_table	
	if atom(data) and data = 0 then	
		data = get(class_scope_table, res)  -- this var appears in methods_scope_table
	end if
	printPop(data[2],data[3])
	return 0
end function

function pushVarPlusExpression(sequence var, sequence exp)
	expression(exp)
	pushVarName(var)
	printf(fhandle , "add\n")
	printf(fhandle , "pop pointer 1\n")	
	printf(fhandle , "push that 0\n")	
	return 0
end function

function letStatement(sequence res)
	if equal(res[3][2],"=") then -- let var = something;
		expression(res[4][2])
		popVarName(res[2][2])
	else							-- let var[exp] = something;
		expression(res[4][2])
		pushVarName(res[2][2])
		printf(fhandle , "add\n")
		expression(res[7][2])
		printPop("temp", 0)
		printPop("pointer", 1)
		printPush("temp", 0)
		printPop("that", 0)
	end if
	return 0
end function

function ifStatement(sequence res)
	integer localCounter = ifCounter
	ifCounter = ifCounter + 1
	
	expression(res[3][2])-- condition
	
	printf(fhandle, "if-goto IF_TRUE")	-- if the condition is TRUE, jump to inside
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
	printf(fhandle, "goto IF_FALSE")		-- if the condition isn't TRUE, jump to FALSE
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
	printf(fhandle, "label IF_TRUE")		-- if TRUE, jump to here
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
	
	ParsingFunc(res[6][2])				-- if TRUE do
	
	printf(fhandle, "goto end")		-- if the condition is TRUE, jump to over "else"
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
	
	printf(fhandle, "label IF_FALSE")	-- if FALSE
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")	
	
	if length(res) > 7 then
		ParsingFunc(res[10])		-- else
	end if
	
	printf(fhandle, "label end")		-- end if
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
	return 0 
end function

function whileStatement(sequence res)
	integer localCounter = ifCounter
	ifCounter = ifCounter + 1
	
	printf(fhandle, "label WHILE_CONDITION") -- beginning of while
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
	
	expression(res[3][2])    		-- condition
	
	printf(fhandle, "not\n")
	printf(fhandle, "if-goto IF_FALSE")  -- condition is FALSE, goto end while
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")

	ParsingFunc(res[6][2])    -- if TRUE do statements

	printf(fhandle, "goto WHILE_CONDITION")  -- goto beginning
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
  
	printf(fhandle, "label IF_FALSE")  -- end while, if condition is FALSE
	printf(fhandle, "%d" , {localCounter})
	printf(fhandle, "\n")
	return 0
end function

function doStatement(sequence res)
	sequence subCall = {}
	for i=2 to length(res) do
		subCall = append(subCall, res[i])
	end for
	
	subroutineCall(subCall)
	printPop("temp", 0)
	return 0
end function

function returnStatement(sequence res)
	sequence temp = res[2][2]
	if sequence(res[2][2]) and equal(res[2][2],";") then
		printPush("constant", 0)
	else
		expression(res[2][2])
	end if
	printf(fhandle , "return\n")
	return 0
end function

function stringConstant(sequence res)
	integer len = length(res)
	printPush("constant", len)
	printf(fhandle , "call String.new 1\n")
	
	for i=1 to len do
		printPush("constant", to_integer(res[i]))
		printf(fhandle , "call String.appendChar 2\n")
	end for
	return 0
end function

function term(sequence res)
	sequence tag = res[1][1]
	switch tag do
		case "integerConstant" then
			printPush("constant", to_integer((res[1][2])))
		case "stringConstant" then
			stringConstant(res[1][2])
		case "keyword" then
			switch res[1][2] do
				case "null" then
					printPush("constant", 0)
				case "true" then
					printPush("constant", 0)
					printf(fhandle , "not \n")
				case "false" then
					printPush("constant", 0)
				case "this" then
					printPush("pointer", 0)
			end switch
		case "identifier" then
			if (length(res) =  1) then	-- varName
				pushVarName(res[1][2])
			else
				object val = res[2][2]
				switch val do
					case "[" then	-- varName [expression]
						pushVarPlusExpression(res[1][2], res[3][2])
					case "." then	-- className|varName.subroutineName(expressionList)
						subroutineCall(res)
					case "(" then	-- subroutineName(expressionList)
						subroutineCall(res)
					case else	-- varName
						pushVarName(res[1][2])
				end switch
			end if
		case "symbol" then
			sequence val = res[1][2]
			switch val do
				case "(" then
					expression(res[2][2])	-- TODO try?
				case "-" then
					term(res[2][2])
					unaryOp(res[1][2])
				case "~" then
					term(res[2][2])
					unaryOp(res[1][2])
			end switch
	end switch
	return 0
end function
					
function subroutineCall(sequence res)
	sequence var = res[2][2]
	switch var do 
		case "." then	-- className|varName.subroutineName(expressionList)
			integer numArg = ceil(length(res[5][2])/2)
			
			object data = get(methods_scope_table, res[1][2])
			if atom(data) and data = 0 then	
				data = get(class_scope_table, res[1][2])
			end if
			
			if sequence(data) then			-- if its a varName
				printPush(data[2],data[3])
				numArg = numArg + 1
				expressionList(res[5][2])	--push parameter list
				printf(fhandle , "call " & data[1] & res[2][2] & res[3][2] & " ")	-- print call aaa.bbb
				printf(fhandle , "%d" , {numArg})
				printf(fhandle , "\n")
			else			
				expressionList(res[5][2])	--push parameter list
				printf(fhandle , "call " & res[1][2] & res[2][2] & res[3][2] & " ")	-- print call aaa.bbb
				printf(fhandle , "%d" , {numArg})
				printf(fhandle , "\n")
			end if
		case "(" then	-- subroutineName(expressionList)
			integer numArg = ceil(length(res[3][2])/2)
			printPush("pointer", 0)
			numArg = numArg + 1
			expressionList(res[3][2])	--push parameter list
			printf(fhandle , "call " & className & "." & res[1][2] & " ")	-- print call aaa.bbb
			printf(fhandle , "%d" , {numArg})
			printf(fhandle , "\n")
	end switch
	return 0
end function

function op(object res)
	sequence var = res
	switch var do
		case "+" then
			printf(fhandle , "add\n")
		case "-" then
			printf(fhandle , "sub\n")
		case "*" then
			printf(fhandle , "call Math.multiply 2\n")
		case "/" then 
			printf(fhandle , "call Math.divide 2\n")
		case "&amp;" then
			printf(fhandle, "and\n")
		case "|" then
			printf(fhandle, "or\n")
		case "&lt;" then
			printf(fhandle, "lt\n")
		case "&gt;" then
			printf(fhandle, "gt\n")
		case "=" then
			printf(fhandle, "eq\n")
		case else
			printf(OUT, "misstake in op()")
	end switch
	return 0
end function

function unaryOp(object res)
	switch res[1] do
		case '-' then
			printf(fhandle , "neg \n")
		case '~' then
			printf(fhandle , "not \n")
		case else
			printf(OUT, "misstake in unaryOp()")
	end switch
	return 0
end function

function expression(sequence res)
	term(res[1][2])
	
	for i=2 to length(res) by 2 do	
		sequence val12 = res[i][2]
		sequence val13 = res[i+1][2]
		term(res[i+1][2])
		op(res[i][2])
	end for
	return 0
end function

function expressionList(sequence res)
	if sequence(res) and equal(res ,{}) then
		-- do nothing	
	else
		expression(res[1][2])
		for i=2 to length(res) by 2 do			
			expression(res[i+1][2])
		end for
	end if
	return 0
end function  

function ParsingFunc(sequence res)  
	if sequence(res) and equal(res ,{}) then		
		-- do nothing	
	--elsif isString(res) then							-- for string value
	
	elsif isString(res[1]) then						-- for tag and sequence values
		switch res[1] do
			case "class" then
				classTag(res[2])
				ParsingFunc(res[2])
			case "classVarDec" then
				classVarDec(res[2])
			case "subroutineDec" then
				SubroutineDec(res[2])
				ParsingFunc(res[2])
			case "parameterList" then
				parameterList(res[2])
			case "varDec" then
				varDec(res[2])
			case "letStatement" then
				PrintSubroutineDec()
				letStatement(res[2])
			case "ifStatement" then
				PrintSubroutineDec()
				ifStatement(res[2])
			case "whileStatement" then
				PrintSubroutineDec()
				whileStatement(res[2])
			case "doStatement" then
				PrintSubroutineDec()
				doStatement(res[2])
			case "returnStatement" then
				PrintSubroutineDec()
				returnStatement(res[2])
			case else
				if sequence(res[2]) and not(isString(res[2])) then	
					for i=1 to length(res[2]) do			
						ParsingFunc(res[2][i])
					end for
				end if
		end switch
	else -- for sequence value
		for i=1 to length(res) do			
			ParsingFunc(res[i])
		end for				
	end if
	return 0
end function

-- Tokenizer xml 
function TokenizingXML(sequence res, integer level=0)  
	if sequence(res) and equal(res ,{}) then-- do nothing
	elsif isString(res) then -- for string value
		printf(fn_xml , res)
	else
		if isString(res[1]) then -- for tag and sequence values
			printf(fn_xml , "<" & res[1] &">")
					
			if not(isString(res[2])) then 
				printf(fn_xml , "\n")
			end if
					
				for i=2 to length(res) do			
					TokenizingXML(res[i], level+1)
				end for

			printf(fn_xml , "<" & "/" & res[1] & ">\n")
		else -- for sequence value
				for i=1 to length(res) do			
					TokenizingXML(res[i], level+1)
				end for				
		end if
	end if
	return 0
end function

function isString(sequence s)  
	integer str = 0
	if sequence(s) and equal(s ,{}) then
		str = 0
	elsif sequence(s) and atom(s[1]) then
		str = 1
	end if
	return str
end function

--Tokenizer xml
function oldTokenizingPrintFunc(sequence res, integer level=0)  
	printf(fn_xml , "<" & res[1] &">")
	
	if (sequence(res[2])and equal(res[2],{})) then
		printf(fn_xml , "\n")
		printTabFunc(level-1)
		printf(fn_xml , "<" & "/" & res[1] & ">")
	elsif (atom(res[2][1])) then
		printf(fn_xml ,( " " & res[2] & " "))
		printf(fn_xml , "<" & "/" & res[1] & ">")
	else
		for i=1 to length(res[2]) do
			printf(fn_xml , "\n")
			printTabFunc(level)
			oldTokenizingPrintFunc(res[2][i], level+1 )
		end for
		printf(fn_xml , "\n")
		printTabFunc(level-1)
		printf(fn_xml , "<" & "/" & res[1] & ">")
	end if	
	return 0
end function

function printTabFunc(integer level)
	for i=0 to level do
		printf(fn_xml, 32 & 32)
	end for
	return 0
end function

--Parsing - Toutput.xml 
function ParsingTXML(sequence res)  
	printf(fn_Txml , "<" & "tokens" &">\n")
		TxmlPrintFunc(res)
	printf(fn_Txml , "<" & "/" & "tokens" & ">")
	return 0
end function

function TxmlPrintFunc(sequence res)  
	if (sequence(res[2])and equal(res[2],{})) then
	elsif (atom(res[2][1])) then
		printf(fn_Txml , "<" & res[1] &">")
		printf(fn_Txml ,( " " & res[2] & " "))
		printf(fn_Txml , "<" & "/" & res[1] & ">\n")
	else
		for i=1 to length(res[2]) do
			TxmlPrintFunc(res[2][i])
		end for
	end if
	return 0
end function

--other functions:
function skipNote()  
  integer last = 0
  current = getc(fn)
  
  while (not (last = '*' and current = '/')) do
	last = current
	current = getc(fn)
  end while
  -- after finding */ read the next char
  nextChar()
  return 0
end function

function skipSpaceEnterTab()
	while (current = ' ' or current = '\n' or current = 9) do
		current = getc(fn)
	end while
	return 0
end function

function skip() --skip in case of comments or space or \n
	while (current = '/' or current = ' ' or current = '\n' or current = 9) do
		skipSpaceEnterTab()
		if current = '/' then
			nextChar()
			switch current do
				-- for: //
				case '/' then
					gets(fn)
					nextChar()
				-- for: /*
				case '*' then
					skipNote()
				case else
					printf(OUT, "error in skip")
			end switch
		end if
	end while
	return 0
end function

function nextChar()
	current = getc(fn)
	skipSpaceEnterTab()
	return 0
end function

function readWord(sequence cond = symbolAscii)
  sequence word = ""  
  
  while (not find(current, cond)) do
	word = word & current
	current = getc(fn)
  end while
  
  skipSpaceEnterTab()
  return word
end function


function StringConstantFunc()
	nextChar()	-- for '"'
	sequence db = {"stringConstant", readWord({'"'})}
	nextChar()	-- for '"'
	return db	
end function

function integerConstantFunc()
	sequence word = ""  
  
  while (find(current, {48,49,50,51,52,53,54,55,56,57})) do
	word = word & current
	current = getc(fn)
  end while
  
  skipSpaceEnterTab()
	return {"integerConstant", word}
end function

function identifierFunc(sequence curWord="")	
	if (equal(curWord, "")) then
		curWord = readWord()
	end if
	return {"identifier", curWord}
end function


function classFunc()
	sequence db = {}
	skip()
	db = append(db,{"keyword", "class"})
	readWord()
	db = append(db, classNameFunc())
	skip()
	db = append(db, {"symbol", "{"})
	nextChar()
	skip()
	db = db & classVarAndSubroutineDec()
	db = append(db, {"symbol", "}"})
	nextChar()
	return {"class", db}
end function

function classVarAndSubroutineDec()
	sequence db = {}
	sequence curWord = readWord()

	while (equal(curWord, "field") or equal(curWord, "static")) do
		db = append(db, classVarDecFunc(curWord))
		skip()
		curWord = readWord()
	end while
	
	while (current != '}') do
		db = append(db, subroutineDecFunc(curWord))
		skip()
		curWord = readWord()
	end while
	
	return db
end function

function classVarDecFunc(sequence curWord)
	sequence db = {}
	
	db = append(db,{"keyword", curWord})
	db = append(db, typeFunc())
	db = append(db, varNameFunc())
		
	while (current != ';') do
		db = append(db, {"symbol", ","})
		nextChar()
		db = append(db, varNameFunc())
	end while
	
	db = append(db,{"symbol", ";"})
	nextChar()
	
	return {"classVarDec", db}
end function

function typeFunc(sequence curWord="")
	
	if (equal(curWord, "")) then
		curWord = readWord()
	end if
	
	if (equal(curWord, "int") or equal(curWord, "char") or equal(curWord, "boolean")) then
		db = {"keyword", curWord}
	else
		db = {"identifier", curWord}
	end if
	
	return db
end function
	
function subroutineDecFunc(sequence curWord)
	sequence db = {}

	db = append(db,{"keyword", curWord})
	curWord = readWord()	
	if (equal(curWord, "void")) then
		db = append(db,{"keyword", "void"})
	else
		db = append(db,typeFunc(curWord))
	end if
	
	db = append(db,subroutineNameFunc())
	db = append(db, {"symbol", "("})
	nextChar()
	
	db = append(db,parameterListFunc())
	
	db = append(db, {"symbol", ")"})
	nextChar()
	db = append(db,subroutineBodyFunc())
	
	return {"subroutineDec", db}
end function

function parameterListFunc()
	sequence db = {}
	
	if (current != ')') then
		db = append(db,typeFunc())
		db = append(db,varNameFunc())

		while (current = ',') do
			db = append(db, {"symbol", ","})
			nextChar()
			db = append(db,typeFunc())
			db = append(db,varNameFunc())
		end while
	end if
	
	return {"parameterList", db}
end function

function subroutineBodyFunc()
	sequence db = {}
	skip()
	db = append(db, {"symbol", "{"})
	nextChar()
	skip()
	
	while (current = 'v') do
		db = append(db, varDecFunc())
	end while
	
	db = append(db, statementsFunc())
	skip()
	db = append(db, {"symbol", "}"})
	nextChar()
	skip()
	return {"subroutineBody", db}
end function

function varDecFunc()
	sequence db = {}
	
	db = append(db, {"keyword", readWord()})
	db = append(db, typeFunc())
	db = append(db, varNameFunc())
	
	while (current != ';') do
		db = append(db, {"symbol", ","})
		nextChar()
		db = append(db, varNameFunc())
	end while
	
	db = append(db,{"symbol", ";"})	
	nextChar()
	
	return {"varDec", db}
end function

function classNameFunc()
	return identifierFunc()
end function

function subroutineNameFunc()
	return identifierFunc()
end function

function varNameFunc()
	return identifierFunc()
end function


function statementsFunc()
	sequence db = { }
	skip()
	-- there is only one option to statement - in subroutineBody, before }
	while (not (current = '}')) do
		db = append(db, statementFunc())
		skip()
	end while

	return {"statements", db}
end function

function statementFunc()
	sequence db = {}
	sequence word = readWord()
	switch word do
		case "let" then
			db = db & letStatementFunc()
		case "if" then
			db = db & ifStatementFunc()
		case "while" then
			db = db & whileStatementFunc()
		case "do" then
			db = db & doStatementFunc()
		case "return" then
			db = db & returnStatementFunc()				
		case else
			printf(OUT, "error in statementFunc")
	end switch
	
return db
end function

function letStatementFunc()
	sequence db = { }
	db = append(db,{"keyword", "let"})
	-- next char was read by readWord()

	db = append(db, varNameFunc())
	
	if not (current = '=') then
		db = append(db, {"symbol","["})
		nextChar()
		
		db = append(db, expressionFunc())
		
		db = append(db, {"symbol","]"})
		nextChar()
	end if
	
	db = append(db,{"symbol", "="})
	nextChar()
	
	db = append(db, expressionFunc())
	
	db = append(db,{"symbol", ";"})
	nextChar()

return {"letStatement", db}
end function

function ifStatementFunc()
	sequence db = { }
	db = append(db,{"keyword", "if"})
	-- readWord in statementsFunc made skipSpaceEnterTab

	db = append(db,{"symbol", "("})
	nextChar()

	db = append(db, expressionFunc())

	db = append(db,{"symbol", ")"})
	nextChar()
	skip() -- in case of if (true) /* ... */  or if (true) //

	db = append(db,{"symbol", "{"})
	nextChar()
	--statementsFunc() doing skip

	db = append(db, statementsFunc())

	db = append(db,{"symbol", "}"})
	nextChar()
	skip() -- in case of if(){} /* ... */  if(){} //
	if (current = 'e') then
		if equal(readWord(),"else") then
			db = append(db, {"keyword","else"})
			db = append(db, {"symbol","{"})
			nextChar()
			
			db = append(db, statementsFunc())
			
			db = append(db, {"symbol","}"})
			nextChar()
			skip() -- in case of else{} /* ... */  else{} //
		end if
	end if
	
return {"ifStatement", db}
end function


function whileStatementFunc()
	sequence db = {}
	db = append(db,{"keyword", "while"})
	-- readWord in statementsFunc made skipSpaceEnterTab

	db = append(db,{"symbol", "("})
	nextChar()

	db = append(db, expressionFunc())

	db = append(db,{"symbol", ")"})
	nextChar()
	skip() -- in case while (true) /* ... */  or while (true) //

	db = append(db,{"symbol", "{"})
	nextChar()
	--statementsFunc() doing skip

	db = append(db, statementsFunc())

	db = append(db,{"symbol", "}"})
	nextChar()
	skip() -- in case if(){} /* ... */  if(){} //
	return {"whileStatement", db}
end function

function doStatementFunc()
	sequence db = {}
	db = append(db,{"keyword", "do"})
	-- next char was read by readWord()
	
	db = db & subroutineCallFunc()	
	
	db = append(db,{"symbol", ";"})
	nextChar()		

return {"doStatement" ,  db}
end function

function returnStatementFunc()
	sequence db = {}
	db = append(db,{"keyword", "return"})
	-- next char was read by readWord()
	
	if not (current = ';') then
		db = append(db, expressionFunc())
	end if
	
	db = append(db,{"symbol", ";"})
	nextChar()		

return {"returnStatement" , db}
end function


function expressionFunc()
sequence db = {}
db = append(db,termFunc())

while not (current = ';' or current = ')' or current = ']' or current = ',') do
		db = append(db, opFunc())
		db = append(db, termFunc())
end while

return {"expression", db}
end function

function termFunc()
	sequence db = {}, word

	if(current = '-' or current = '~') then
		db = append(db, unaryOpFunc())
		db = append(db, termFunc())
	elsif(current >= 48 and current <= 57)then
		db = append(db, integerConstantFunc())
	elsif(current = '"')then
		db = append(db, StringConstantFunc())
	elsif(current = '(')then
		db = append(db, {"symbol", "("})
		nextChar()
		db = append(db, expressionFunc())
		db = append(db,{"symbol", ")"})
		nextChar()
	else
		word = readWord()
		if(find (word,keywordConstant)) then
			db = append(db, keywordConstantFunc(word))
		elsif(current = '(') or (current = '.') then
			db = db & subroutineCallFunc(word)
		elsif(current = '[')then
			db = append(db,identifierFunc(word))
			db = append(db,{"symbol", "["})
			nextChar()
			db = append(db, expressionFunc())
			db = append(db,{"symbol", "]"})
			nextChar()
		else
			db = append(db,identifierFunc(word))
		end if
	end if
	
return {"term", db}
end function

function subroutineCallFunc(sequence word = "")
	if equal(word,"") then
		word = readWord()
	end if
	
	sequence db = {identifierFunc(word)}
	if(current = '.') then
		db = append(db,{"symbol", "."})
		nextChar()
		db = append(db, identifierFunc())
	end if

	db = append(db,{"symbol", "("})
		nextChar()
		db = append(db, expressionListFunc())
		db = append(db,{"symbol", ")"})
		nextChar()
	return db
end function

function expressionListFunc()
	sequence db = {}
	if( not (current = ')')) then
		db = append(db, expressionFunc())
		while not (current = ')') do
			db = append(db, {"symbol", ","})
			nextChar()
			db = append(db, expressionFunc())
		end while
	end if
return {"expressionList", db}
end function

function opFunc()-- only space can be before the operator.
-- read the next char when finished
sequence db = {}
	switch current do
		case '+' then
			db = db & "+"
		case '-' then
			db = db & "-"
		case '*' then
			db = db & "*"
		case '/' then 
			db = db & "/"
		case '&' then
			db = db & "&amp;"	
		case '|' then
			db = db & "|"
		case '<' then
			db = db & "&lt;"
		case '>' then
			db = db & "&gt;"
		case '=' then
			db = db & "="				
		case else
			printf(OUT, "misstake in opFunc()")
	end switch

-- read the next char
nextChar()
return {"symbol" ,db}
end function

function unaryOpFunc()-- only space can be before the operator.
-- read the next char when finished
sequence db = {}
skipSpaceEnterTab()
	switch current do
		case '-' then
			db = db & "-"
		case '~' then
			db = db & "~"
		case else
			printf(OUT, "misstake in unaryOpFunc()")
	end switch

-- read the next char
nextChar()
return {"symbol" , db}
end function

function keywordConstantFunc(sequence word = "")
	if equal(word,"") then
		word = readWord()
	end if
return {"keyword", word}
end function

function look_at(sequence path_name, sequence item)
	if find('d', item[D_ATTRIBUTES]) then
		if find('s', item[D_ATTRIBUTES]) then
			return W_SKIP_DIRECTORY -- Don't recurse a system directory
			else
			return 0 -- Keep processing as normal
		end if
	end if
	if not find(fileext(item[D_NAME]), {"jack"}) then
		return 0 -- ignore non vm files
	end if
	data = keyvalues(item[D_NAME], "", "", "", ".", 0)
	--fileName=data[1]
	fname = data[1] & ".vm"
	fhandle = open(fname, "w")
	if fhandle = -1 then
		printf(1, "Couldn't create output file!%s\n", data[1])
	end if
	fn = open(item[D_NAME], "r")
	if fn = -1 then
		printf(1, "Can't open %s\n", item[D_NAME])
		abort(1)
    end if 
	
	nextChar()
    sequence db = classFunc()
	ParsingFunc(db)
	
	close(fn)--close the input jack file
	close(fhandle)--close the output vm file

  return 0    
end function-- keep going

--trace(1)
object exit_code = walk_dir(path, routine_id("look_at"), TRUE)	
display("\n")