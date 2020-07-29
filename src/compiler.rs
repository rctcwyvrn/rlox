use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{OpCode, Instr, Chunk, FunctionChunk, FunctionType};
use crate::value::Value;
use crate::prec::{Precedence, ParseFn, get_rule};
use crate::debug::disassemble_chunk;
use crate::resolver::{Resolver};

pub const DEBUG: bool = true;

#[derive(Debug)]
pub struct Compiler<'a> {
    scanner: Scanner<'a>,
    tokens: Vec<Token>,

    functions: Vec<FunctionChunk>,
    current_function: usize, // The current FunctionChunk
    parent_functions: Vec<usize>, // Which FunctionChunk should the the compiler return to after. Acts as a stack

    resolver: &'a mut Resolver, // Manages the slots for the local variables and upvalues, represented as a Vec of individal ResolverNodes

    had_error: bool,
    panic_mode: bool,
}

impl Compiler<'_> {
    fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.functions.get_mut(self.current_function).unwrap().chunk
    }

    fn current_chunk_ref(&self) -> &Chunk {
        &self.functions.get(self.current_function).unwrap().chunk
    }

    fn current_fn(&mut self) -> &mut FunctionChunk {
        self.functions.get_mut(self.current_function).unwrap()
    }

    fn current_fn_type(&self) -> FunctionType {
        self.functions.get(self.current_function).unwrap().fn_type
    }

    fn advance(&mut self){
        self.tokens.push(self.scanner.scan_token()); // Wastes memory by not just dropping the older tokens, make advance() drop older tokens after i finish the code?
        if self.current().token_type == TokenType::TokenError {
            self.error("Error in scanning");
            self.advance();
        }
        //println!("depth = {} | prev {:?} | cur {:?}", self.current_function, self.previous(), self.current());
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.tokens.len() - 2]
    }

    fn current(&self) -> &Token {
        &self.tokens[self.tokens.len() - 1]
    }

    fn consume(&mut self, token_type: TokenType, msg: &str) {
        if self.current().token_type == token_type {
            self.advance();
        } else {
            self.error(msg);
        }
    }

    fn match_cur(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        } else {
            self.advance();
            return true;
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current().token_type == token_type
    }

    fn error(&mut self, message: &str) {
        if self.panic_mode { return } // Ignore other errors while in panic_mode
        let token = self.previous();
        eprintln!("[Line {}] Error", token.line_num);
        match token.token_type {
            TokenType::TokenEOF => eprintln!(" at end of file"),
            TokenType::TokenError => (), // nothing
            _ => eprint!(" at '{}'", token.lexemme)
        }

        eprintln!(": {}", message);

        self.had_error = true;
        self.panic_mode = true;
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while !self.check(TokenType::TokenEOF) {
            if self.previous().token_type == TokenType::TokenSemicolon { return; }
            match self.current().token_type {
                TokenType::TokenClass | TokenType::TokenFun | TokenType::TokenVar | TokenType::TokenFor | TokenType::TokenIf | TokenType::TokenWhile | TokenType::TokenPrint | TokenType::TokenReturn => return,
                _ => (),
            }
            self.advance();
        }
    }

    fn emit_instr(&mut self, op_code: OpCode) {
        // println!("Emitting instr {:?} from token {:?}", op_code, self.previous()); kinda useful
        let instr = Instr {
            op_code,
            line_num: self.previous().line_num
        };
        self.current_chunk().write_instruction(instr)
    }

    fn emit_instrs(&mut self, op_codes: &[OpCode]) {
        for oc in op_codes {
            self.emit_instr(*oc)
        }
    }

    fn emit_constant(&mut self, value: Value) -> usize {
        let index = self.current_chunk().add_constant(value);
        self.emit_instr(OpCode::OpConstant(index));
        index
    }

    fn emit_return(&mut self) {
        self.emit_instrs(&[OpCode::OpNil, OpCode::OpReturn]);
    }

    /// Emits OpCode::OpJump
    fn emit_jump(&mut self) -> usize {
        self.emit_instr(OpCode::OpJump(usize::max_value())); 
        self.current_chunk().code.len() - 1
    }

    /// Emits OpCode::OpJumpIfFalse
    fn emit_jif(&mut self) -> usize {
        self.emit_instr(OpCode::OpJumpIfFalse(usize::max_value())); 
        self.current_chunk().code.len() - 1
    }

    /// Given the index of the jump instruction in the chunk, update the opcode to jump to the instruction after the current one
    fn patch_jump(&mut self, offset: usize) {
        let jump_amount = self.current_chunk().code.len() - offset - 1;
        if jump_amount > usize::max_value() {
            self.error("Too much code to jump over.");
        }
          
        let jump_instr = self.current_chunk().code.get_mut(offset).unwrap();
        macro_rules! replace_jump {
            ($jump_type: path) => {{jump_instr.op_code = $jump_type(jump_amount)}}
        }

        match jump_instr.op_code {
            OpCode::OpJump(_) => replace_jump!(OpCode::OpJump),
            OpCode::OpJumpIfFalse(_) => replace_jump!(OpCode::OpJumpIfFalse),
            _ => {
                let instr = self.current_chunk_ref().code.get(offset).unwrap().clone();
                self.error(&format!("Attempted to patch a non_jump op code instruction: {:?}", instr));
            },
        }
    }

    /// loop_start: Index of the instruction to jump back to
    fn emit_loop(&mut self, loop_start: usize) {
        let loop_op = OpCode::OpLoop(self.current_chunk().code.len() - loop_start);
        self.emit_instr(loop_op);
    }

    /// Emits an OpReturn
    fn end_compilation(&mut self) {
        self.emit_return();
    }

    /// End scope by emitting pop instructions and cleaning the resolver
    fn end_scope(&mut self) {
        for _ in 0..self.resolver.end_scope() {
            self.emit_instr(OpCode::OpPop); // Remove old local variables
        }
    }

    /// Calls Resolver::declare_variable() with the previous Token's lexemme (TokenIdentifier) 
    fn declare_variable(&mut self) {
        let str_val = self.previous().lexemme.clone();
        let success = self.resolver.declare_variable(str_val);
        if !success {
            self.error("Variable with this name already declared in this scope"); 
        }
    }

    fn parse_precedence(&mut self, prec: Precedence) {
        self.advance();

        // Parse the start of the prefix expression
        // We know this must be a prefix because we can't start with something that is an infix (eg + 3 2)
        let prefix_rule = get_rule(self.previous().token_type).prefix;
        // Used only by variable() to determine if a TokenIdentifier is for an assignment or get
        let can_assign = prec <= Precedence::PrecAssignment;
        self.call_parse_fn(prefix_rule, can_assign);

        // Parse any number of infix expressions, as long as they have higher precedence
        while prec <= get_rule(self.current().token_type).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous().token_type).infix;
            self.call_parse_fn(infix_rule, can_assign);
        }

        // Show compilation error for a TokenEqual found in an infix position
        // Note: I think this just does not work at all, maybe we need to match against previous?
        if can_assign && self.match_cur(TokenType::TokenEqual) {
            self.error("Invalid assignment target");
        }
    }
    
    fn call_parse_fn(&mut self, parse_fn: ParseFn, can_assign: bool) {
        match parse_fn {
            ParseFn::None       => self.error("Expected expression"),
            ParseFn::Binary     => self.binary(),
            ParseFn::Grouping   => self.grouping(),
            ParseFn::Unary      => self.unary(),
            ParseFn::Number     => self.number(),
            ParseFn::Literal    => self.literal(),
            ParseFn::String     => self.string(),
            ParseFn::Variable   => self.variable(can_assign),
            ParseFn::And        => self.and_operator(),
            ParseFn::Or         => self.or_operator(),
            ParseFn::Call       => self.call(),
            ParseFn::Dot        => self.dot(can_assign),
        }
    }

    fn declaration(&mut self) {
        if self.match_cur(TokenType::TokenFun) {
            self.fun_declaration();
        } else if self.match_cur(TokenType::TokenClass) {
            self.class_declaration();
        } else if self.match_cur(TokenType::TokenVar) {
            self.var_declaration();
        } else {
            self.statement();
        }
        if self.panic_mode { self.synchronize(); }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expected function name");
        self.resolver.mark_initialized(); // Initialize the function object if we are in a local scope
        self.function(FunctionType::Function);
        self.define_variable(global); // Emit the define instr if we are in the global scope
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::TokenIdentifier, "Expected class name after keyword 'class'");
        let name = self.previous().lexemme.clone();
        let name_index = self.identifier_constant(&name);
        self.declare_variable();

        self.emit_instr(OpCode::OpClass(name_index));
        self.define_variable(name_index);

        self.consume(TokenType::TokenLeftBrace, "Expected '{' before class body");
        self.consume(TokenType::TokenRightBrace, "Expected '}' after class body");
    }

    // Note: Since this constantly confuses me, I'm gonna keep a note here so that I don't forget how variables work in rlox
    // Globals: The opcodes GetGlobal and SetGlobal take a LoxString from the constants vec and map it into a HashMap in the VM, no resolving/checking is done before runtime
    // Locals: Local variables live on the stack and since they are the ONLY values that do not get popped after statements, we know that they must live at the very bottom of the stack, 
    // and thus we can just raw index from the bottom of the stack to the index of the variable by looking at how many locals have been defined in this scope
    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expected variable name");
        if self.match_cur(TokenType::TokenEqual) {
            self.expression();
        } else {
            self.emit_instr(OpCode::OpNil);
        }
        self.consume(TokenType::TokenSemicolon, "Expected ';' after variable declaration");
        self.define_variable(global);
    }

    /// Match the identifier token and pass it into identifier_constant to be added to the chunk if current scope is global
    /// 
    /// Calls declare_variable() if the current scope is local
    fn parse_variable(&mut self, error_msg: &str) -> usize {
        self.consume(TokenType::TokenIdentifier, error_msg);
        self.declare_variable();

        if self.resolver.is_global() {
            let str_val = self.previous().lexemme.clone();
            self.identifier_constant(&str_val)
        } else {
            0
        }
    }

    /// Add a string to the chunk as a constant and return the index
    /// 
    /// Only used for global variables
    fn identifier_constant(&mut self, str_val: &String) -> usize {
        self.current_chunk().add_constant(Value::LoxString(str_val.to_string()))
    }

    /// Emits the instruction to define the global variable
    /// or to set the local variable as initialized
    fn define_variable(&mut self, global: usize) {
        if self.resolver.is_global() {
            self.emit_instr(OpCode::OpDefineGlobal(global));
        } else {
            self.resolver.mark_initialized();
        }
    }

    fn statement(&mut self) {
        if self.match_cur(TokenType::TokenPrint) {
            self.print_statement();
        } else if self.match_cur(TokenType::TokenReturn) {
            self.return_statement();
        } else if self.match_cur(TokenType::TokenIf) {
            self.if_statement();
        } else if self.match_cur(TokenType::TokenWhile) {
            self.while_statement();
        } else if self.match_cur(TokenType::TokenFor) {
            self.for_statement();
        } else if self.match_cur(TokenType::TokenLeftBrace) {
            self.resolver.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::TokenSemicolon, "Expected ';' after value in print statement");
        self.emit_instr(OpCode::OpPrint);
    }

    fn return_statement(&mut self) {
        if self.current_fn_type() == FunctionType::Script {
            self.error("Cannot return from top-level code");
        }

        if self.match_cur(TokenType::TokenSemicolon) {
            // Nil return
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::TokenSemicolon, "Expected ';' after return value");
            self.emit_instr(OpCode::OpReturn);
        }
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::TokenLeftParen, "Expected '(' after 'if'");
        self.expression();
        self.consume(TokenType::TokenRightParen, "Expected ')' after condition");

        // Keep track of where we put the first conditional jump
        let jump_index = self.emit_jif();

        self.emit_instr(OpCode::OpPop); // Pop off the if conditional in the 'then' case
        self.statement(); // Then case
        let else_jump = self.emit_jump(); // Keep track of where we put the jump to go over the else statement
        self.patch_jump(jump_index);

        if self.match_cur(TokenType::TokenElse) {
            self.emit_instr(OpCode::OpPop); // Pop off the if conditional if we jump over the 'then' case
            self.statement(); // Else case
        }
        self.patch_jump(else_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len() - 1;
        
        self.consume(TokenType::TokenLeftParen, "Expected '(' after 'while'");
        self.expression();
        self.consume(TokenType::TokenRightParen, "Expected ')' after loop condition");

        let exit_jump = self.emit_jif();

        self.emit_instr(OpCode::OpPop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit_instr(OpCode::OpPop);
    }

    fn for_statement(&mut self) {
        self.consume(TokenType::TokenLeftParen, "Expected '(' after 'for'");
        
        self.resolver.begin_scope();

        // First clause: Can be var declaration or expresion
        if self.match_cur(TokenType::TokenSemicolon) {
            // Do nothing
        } else if self.match_cur(TokenType::TokenVar) {
            self.var_declaration();
        } else {
            self.expression_statement(); //
        }

        let mut loop_start = self.current_chunk().code.len() - 1; // Loop should include 2nd and 3rd clauses (if they exist)
        let mut exit_jump = None;

        // Loop conditional
        if !self.match_cur(TokenType::TokenSemicolon) {
            self.expression();
            self.consume(TokenType::TokenSemicolon, "Expected ';' after loop condition");
            exit_jump = Some(self.emit_jif());
            self.emit_instr(OpCode::OpPop); // Pop condition if we didn't jump
        } // Note: if this conditional is not found, then there is no way to jump out of the loop

        if !self.match_cur(TokenType::TokenRightParen) {
            // Jump to body, set this point to be the one to loop back to after executing the body, jump to next iteration
            let body_jump = self.emit_jump(); // Jump to after the increment and the loop

            let increment_start = self.current_chunk().code.len() - 1;
            self.expression(); // Parse the increment expression
            self.emit_instr(OpCode::OpPop); // Pop the remaining value
            self.consume(TokenType::TokenRightParen, "Expected ')' after for loop clauses");

            self.emit_loop(loop_start); // Loop back to the start after increment
            loop_start = increment_start; // Make the body go to the start of the increment instead of the start of the loop

            self.patch_jump(body_jump); // Patching up the body jump
        }

        self.statement();
        self.emit_loop(loop_start);

        if let Some(offset) = exit_jump {
            self.patch_jump(offset);
            self.emit_instr(OpCode::OpPop);
        }

        self.end_scope();
    }

    fn block(&mut self) {
        while !self.check(TokenType::TokenRightBrace) && !self.check(TokenType::TokenEOF) {
            self.declaration();
        }
        self.consume(TokenType::TokenRightBrace, "Expected '}' after block"); // Fails if we hit EOF instead
    }

    /// Compiles the function into a new FunctionChunk, adds it to the current parser, adds the LoxFunction object to the constants stack, returns the OpConstant pointing to it
    fn function(&mut self, fun_type: FunctionType) {
        //let mut function_parser = self.from_old(fun_type);

        let index = self.start_child(fun_type);
        self.resolver.begin_scope();

        self.consume(TokenType::TokenLeftParen, "Expected '(' after function name");
        if !self.check(TokenType::TokenRightParen) {
            loop {
                let cur_function = self.current_fn();
                cur_function.arity += 1;
                if cur_function.arity > 255 {
                    self.error("Cannot have more than 255 parameters");
                }

                let param_constant = self.parse_variable("Expected parameter name");
                self.define_variable(param_constant);
                if !self.match_cur(TokenType::TokenComma) { break; }
            }
        }
        self.consume(TokenType::TokenRightParen, "Expected ')' after function parameters");
        
        self.consume(TokenType::TokenLeftBrace, "Expected '{' before function body");
        self.block();        
        self.end_child();

        self.emit_constant(Value::LoxFunction(index));
        self.emit_instr(OpCode::OpClosure);
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::TokenSemicolon, "Expected ';' after value");
        self.emit_instr(OpCode::OpPop);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::PrecAssignment)
    }

    fn and_operator(&mut self) {
        let end_jump = self.emit_jif();

        self.emit_instr(OpCode::OpPop);
        self.parse_precedence(Precedence::PrecAnd); // Parse right hand side of the infix expression
        self.patch_jump(end_jump); // Jump to after it if the first argument was already false, leaving the false value on the top of the stack to be the result
        // Otherwise the first argument is true, so the value of the whole and is equal to the value of the second argument, so just proceed as normal
    }

    fn or_operator(&mut self) {
        // If false then execute the other expression
        let else_jump = self.emit_jif();

        // If the first one is already truthy, go to the end
        let end_jump = self.emit_jump();

        self.patch_jump(else_jump);
        self.emit_instr(OpCode::OpPop);
        self.parse_precedence(Precedence::PrecOr);

        self.patch_jump(end_jump);
    }

    fn number(&mut self) {
        let value = self.previous().lexemme.parse::<f64>().unwrap(); // sketch but ok
        self.emit_constant(Value::Double(value));
    }

    fn literal(&mut self) {
        match self.previous().token_type {
            TokenType::TokenFalse => self.emit_instr(OpCode::OpFalse),
            TokenType::TokenTrue => self.emit_instr(OpCode::OpTrue),
            TokenType::TokenNil => self.emit_instr(OpCode::OpNil),
            _ => panic!("Unreachable state reached, attempted to make a literal out of a non-literal type?"),
        }
    }

    fn string(&mut self) {
        let str_val = self.previous().lexemme.clone();
        self.emit_constant(Value::LoxString(unwrap_string(&str_val)));
    }

    fn variable(&mut self, can_assign: bool) {
        let name = &self.previous().lexemme.clone();
        self.named_variable(name, can_assign)
    }

    // Note: parse_precedence with TokenIdentifier => variable() -> named_variable(previous.lexemme) 
    // Could be a getter or a setter, so lookahead for a '='
    fn named_variable(&mut self, name: &String, can_assign: bool) {
        let local_arg = match self.resolver.resolve_local(name) {
            Ok(opt) => opt,
            Err(_) => { 
                self.error("Cannot read local variable in its own initializer"); 
                return; 
            },
        };

        // Figure out which type of get/set OpCodes we want
        let (get_op, set_op) = if let Some(local_index) = local_arg  {
            (OpCode::OpGetLocal(local_index), OpCode::OpSetLocal(local_index))

        } else if let Some(upvalue_index) = self.resolver.resolve_upvalue(name) {
            (OpCode::OpGetUpvalue(upvalue_index), OpCode::OpSetUpvalue(upvalue_index))

        } else {
            let global_arg = self.identifier_constant(name); // Does NOT check at compile time if this variable can be resolved
            (OpCode::OpGetGlobal(global_arg), OpCode::OpSetGlobal(global_arg))
        };

        // Figure out if we want to use the get or the set from the pair of possible ops we determined earlier
        if self.match_cur(TokenType::TokenEqual) && can_assign {
            self.expression();
            self.emit_instr(set_op);
        } else {
            self.emit_instr(get_op);
        }
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::TokenRightParen, "Expected ')' after expression");
    }

    fn unary(&mut self) { 
        let operator_type = self.previous().token_type;
        self.parse_precedence(Precedence::PrecUnary); // evaluate the expression in the unary
        match operator_type {
            TokenType::TokenMinus => self.emit_instr(OpCode::OpNegate),
            TokenType::TokenBang => self.emit_instr(OpCode::OpNot),
            _ => () // Error?
        }
    }

    fn binary(&mut self) {
        let operator_type = self.previous().token_type;

        let rule = get_rule(operator_type);
        self.parse_precedence(rule.next_precedence());

        // Stack based vm, so emit the binary instr after
        match operator_type {
            TokenType::TokenPlus        => self.emit_instr(OpCode::OpAdd),
            TokenType::TokenMinus       => self.emit_instr(OpCode::OpSubtract),
            TokenType::TokenStar        => self.emit_instr(OpCode::OpMultiply),
            TokenType::TokenSlash       => self.emit_instr(OpCode::OpDivide),
            TokenType::TokenBangEqual   => self.emit_instrs(&[OpCode::OpEqual, OpCode::OpNot]),
            TokenType::TokenEqualEqual  => self.emit_instr(OpCode::OpEqual),
            TokenType::TokenGreater     => self.emit_instr(OpCode::OpGreater),
            TokenType::TokenGreaterEqual => self.emit_instrs(&[OpCode::OpLess, OpCode::OpNot]),
            TokenType::TokenLess        => self.emit_instr(OpCode::OpLess),
            TokenType::TokenLessEqual   => self.emit_instrs(&[OpCode::OpGreater, OpCode::OpNot]),
            _ => () // error?
        }
    }

    /// Infix operation for function calls, assumes that the LoxFunction will be at the top of the stack when this is called, usually
    /// via a global/local variable resolution
    fn call(&mut self) {
        let arg_count = self.argument_list();
        self.emit_instr(OpCode::OpCall(arg_count))
    }

    /// Parses expressions while looking for commas inbetween and for the closing paren. Leaves the values on the stack
    fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(TokenType::TokenRightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("Cannot have more than 255 arguments");
                }
                arg_count += 1;

                if !self.match_cur(TokenType::TokenComma) { break; }
            } 
        }
        self.consume(TokenType::TokenRightParen, "Expected ')' after function argument list");
        arg_count
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::TokenIdentifier, "Expected property name after '.'");
        let name_index = self.identifier_constant(&self.previous().lexemme.clone());
        
        if can_assign && self.match_cur(TokenType::TokenEqual) { // We check can_assign so that a + b.c = 3 does not invalidly emit a set op
            // Setter
            self.expression();
            self.emit_instr(OpCode::OpSetProperty(name_index));
        } else {
            self.emit_instr(OpCode::OpGetProperty(name_index));
        }
    }

    /// Sets the compiler to generate a new function chunk for the next segment of code
    fn start_child(&mut self, function_type: FunctionType) -> usize {
        let function_name = self.previous().lexemme.clone();
        self.functions.push(FunctionChunk::new(Some(function_name), 0, function_type));
        self.resolver.push();
        self.parent_functions.push(self.current_function);
        self.current_function = self.functions.len() - 1;

        self.functions.len() - 1 
    }

    /// Switches the current chunk out of the new function def
    fn end_child(&mut self) {
        // Emit an implicit nil return if not specified explicity
        if self.current_chunk_ref().code.last().unwrap().op_code != OpCode::OpReturn {
            self.emit_return();
        }
        let upvalues = self.resolver.pop();
        self.current_fn().set_upvalues(upvalues);
        self.current_function = self.parent_functions.pop().unwrap();
    }

    pub fn new<'a>(code: &'a String, resolver: &'a mut Resolver) -> Compiler<'a> {
        let mut scanner = Scanner::new(code);

        let mut tokens = Vec::new();
        tokens.push(scanner.scan_token()); // Load up the first token

        let mut functions = Vec::new();
        functions.push(FunctionChunk::new(None, 0, FunctionType::Script)); // Start the compilation with a top level function

        Compiler {
            scanner,
            tokens,
            functions,
            current_function: 0,
            parent_functions: Vec::new(),
            resolver,
            had_error: false,
            panic_mode: false,
        }
    }

    // Note: is this an expensive move (moving self into this function) ? Is it less expensive to just move/copy the FunctionChunks afterwards?
    pub fn compile(mut self) -> Option<Vec<FunctionChunk>> { 
        while !self.match_cur(TokenType::TokenEOF) {
            self.declaration();
        }
        self.end_compilation();

        if DEBUG {
            for fn_chunk in self.functions.iter() {
                disassemble_chunk(&fn_chunk.chunk, &fn_chunk.name);
            }
        }

        if !self.had_error { Some(self.functions) } else { None }
    }
}

// Removes the surrounding "" around TokenString lexemmes
fn unwrap_string(s: &str) -> String {
    s[1..s.len() -1].to_string()
}