// cobolInterpreter.tsx

/**
 * Represents a collection of variables with their corresponding values.
 */
type Variables = { [key: string]: any };

/**
 * Represents the execution environment, including variables and their data types.
 */
type Environment = {
  variables: Variables;
  dataTypes: { [key: string]: string };
};

/**
 * Represents the result of interpreting COBOL code.
 */
interface InterpreterResult {
  output: string;
  variables: Variables;
}

/**
 * Main function to interpret COBOL code.
 * @param code - The COBOL code as a string.
 * @returns The result of interpretation, including output and variable states.
 */
export function cobolInterpreter(code: string): InterpreterResult {
  let output: string = "";
  const environment: Environment = {
    variables: {},
    dataTypes: {},
  };

  try {
    // Split the code into its respective divisions
    const divisions = splitDivisions(code);

    // Process DATA DIVISION
    if (divisions["DATA DIVISION"]) {
      parseDataDivision(divisions["DATA DIVISION"], environment);
    }

    // Process ENVIRONMENT DIVISION
    if (divisions["ENVIRONMENT DIVISION"]) {
      parseEnvironmentDivision(divisions["ENVIRONMENT DIVISION"], environment);
    }

    // Process PROCEDURE DIVISION
    if (divisions["PROCEDURE DIVISION"]) {
      const lines = preprocessCode(divisions["PROCEDURE DIVISION"]);
      executeProcedureDivision(lines, environment, (text) => {
        output += text + "\n";
      });
    } else {
      throw new Error("Missing PROCEDURE DIVISION.");
    }
  } catch (error: any) {
    output += `Error: ${error.message}`;
  }

  return { output: output.trim(), variables: environment.variables };
}

/**
 * Splits the COBOL code into its respective divisions.
 * @param code - The entire COBOL code.
 * @returns An object mapping division names to their content.
 */
function splitDivisions(code: string): { [key: string]: string } {
  const divisionRegex =
    /(IDENTIFICATION DIVISION\.|ENVIRONMENT DIVISION\.|DATA DIVISION\.|PROCEDURE DIVISION\.)/gi;
  const parts = code.split(divisionRegex);
  const divisions: { [key: string]: string } = {};

  for (let i = 1; i < parts.length; i += 2) {
    const divisionName = parts[i].replace(".", "").trim().toUpperCase();
    const divisionContent = parts[i + 1] || "";
    divisions[divisionName] = divisionContent;
  }
  return divisions;
}

/**
 * Parses the DATA DIVISION to initialize variables and their data types.
 * @param dataDivision - Content of the DATA DIVISION.
 * @param environment - The execution environment.
 */
function parseDataDivision(dataDivision: string, environment: Environment) {
  const lines = preprocessCode(dataDivision);
  for (const line of lines) {
    const dataMatch = line.match(/^(\d+)\s+(\w+)\s+PIC\s+([\w()]+)/i);
    if (dataMatch) {
      const level = parseInt(dataMatch[1]);
      const varName = dataMatch[2];
      const picClause = dataMatch[3];
      environment.variables[varName] = initializeVariable(picClause);
      environment.dataTypes[varName] = picClause;
    } else if (line.trim()) {
      throw new Error(`Invalid DATA DIVISION line: "${line}"`);
    }
  }
}

/**
 * Initializes a variable based on its PIC clause.
 * @param picClause - The PIC clause defining the variable's data type.
 * @returns The initialized value of the variable.
 */
function initializeVariable(picClause: string): any {
  // Simplified initialization based on PIC clause
  if (picClause.startsWith("9")) {
    return 0; // Numeric
  } else if (picClause.startsWith("X")) {
    return ""; // Alphanumeric
  }
  return null;
}

/**
 * Parses the ENVIRONMENT DIVISION to simulate file handling.
 * @param envDivision - Content of the ENVIRONMENT DIVISION.
 * @param environment - The execution environment.
 */
function parseEnvironmentDivision(
  envDivision: string,
  environment: Environment
) {
  const lines = preprocessCode(envDivision);
}

/**
 * Preprocesses code by removing comments and empty lines.
 * @param codeSection - A section of COBOL code.
 * @returns An array of cleaned code lines.
 */
function preprocessCode(codeSection: string): string[] {
  return codeSection
    .split(/\r?\n/)
    .map((line) => line.split("*")[0].trim()) // Remove comments starting with '*'
    .filter((line) => line !== "");
}

/**
 * Executes the PROCEDURE DIVISION by interpreting each line.
 * @param lines - The lines of code in the PROCEDURE DIVISION.
 * @param environment - The execution environment.
 * @param outputCallback - Callback to handle output.
 * @param idxRef - Reference object to keep track of the current index.
 */
function executeProcedureDivision(
  lines: string[],
  environment: Environment,
  outputCallback: (text: string) => void,
  idxRef: { idx?: number } = {}
) {
  let idx = idxRef.idx || 0;

  while (idx < lines.length) {
    const line = lines[idx];
    idxRef.idx = idx;

    // Handle DISPLAY statements
    if (line.startsWith("DISPLAY")) {
      const contentMatch = line.match(/DISPLAY\s+(.*)/i);
      if (contentMatch) {
        const content = contentMatch[1];
        const evaluated = evaluateExpression(content, environment);
        outputCallback(String(evaluated));
      } else {
        throw new Error("Invalid DISPLAY statement.");
      }
      idx++;
    }
    // Handle ACCEPT statements (input)
    else if (line.startsWith("ACCEPT")) {
      const varNameMatch = line.match(/ACCEPT\s+(\w+)/i);
      if (varNameMatch) {
        const varName = varNameMatch[1];
        // Simulate input (In actual implementation, replace with input handling)
        // For security reasons, 'prompt' is not available in some environments
        // Here, we simulate input as an empty string or a predefined value
        environment.variables[varName] = ""; // Placeholder for input
      } else {
        throw new Error("Invalid ACCEPT statement.");
      }
      idx++;
    }
    // Handle MOVE statements (assignment)
    else if (line.startsWith("MOVE")) {
      const moveMatch = line.match(/MOVE\s+(.*)\s+TO\s+(\w+)/i);
      if (moveMatch) {
        const valueExpr = moveMatch[1];
        const varName = moveMatch[2];
        const value = evaluateExpression(valueExpr, environment);
        environment.variables[varName] = value;
      } else {
        throw new Error("Invalid MOVE statement.");
      }
      idx++;
    }
    // Handle arithmetic operations
    else if (/^(ADD|SUBTRACT|MULTIPLY|DIVIDE)/i.test(line)) {
      handleArithmeticOperation(line, environment);
      idx++;
    }
    // Handle IF statements
    else if (line.startsWith("IF")) {
      idx = handleIfStatement(lines, idx, environment, outputCallback);
    }
    // Handle PERFORM loops
    else if (line.startsWith("PERFORM")) {
      idx = handlePerformLoop(lines, idx, environment, outputCallback);
    }
    // Handle EVALUATE statements (Switch-case)
    else if (line.startsWith("EVALUATE")) {
      idx = handleEvaluateStatement(lines, idx, environment, outputCallback);
    }
    // Handle STRING statements
    else if (line.startsWith("STRING")) {
      idx = handleStringStatement(lines, idx, environment);
    }
    // Handle UNSTRING statements
    else if (line.startsWith("UNSTRING")) {
      idx = handleUnstringStatement(lines, idx, environment);
    }
    // Handle STOP RUN
    else if (/^STOP\s+RUN\.?$/i.test(line)) {
      break;
    }
    // Handle other statements or labels
    else {
      idx++;
    }
  }
  idxRef.idx = idx;
}

/**
 * Handles arithmetic operations such as ADD, SUBTRACT, MULTIPLY, and DIVIDE.
 * @param line - The line containing the arithmetic operation.
 * @param environment - The execution environment.
 */
function handleArithmeticOperation(line: string, environment: Environment) {
  const [_, operation, operandsPart] =
    line.match(/(ADD|SUBTRACT|MULTIPLY|DIVIDE)\s+(.*)/i) || [];
  if (operation && operandsPart) {
    const operands = operandsPart.split(/\s+TO\s+/i);
    const operandValues = operands[0]
      .split(/\s+AND\s+/i)
      .map((op) => evaluateExpression(op, environment));

    const varNameMatch = operands[1]?.match(/(\w+)/);
    if (varNameMatch) {
      const varName = varNameMatch[1];
      let result = environment.variables[varName] || 0;

      for (const value of operandValues) {
        switch (operation.toUpperCase()) {
          case "ADD":
            result += value;
            break;
          case "SUBTRACT":
            result -= value;
            break;
          case "MULTIPLY":
            result *= value;
            break;
          case "DIVIDE":
            if (value === 0) {
              throw new Error("Division by zero.");
            }
            result /= value;
            break;
        }
      }
      environment.variables[varName] = result;
    } else {
      throw new Error(`Invalid variable in ${operation} statement.`);
    }
  } else {
    throw new Error(`Invalid ${operation} statement.`);
  }
}

/**
 * Handles IF statements, including nested IFs with ELSE and END-IF.
 * @param lines - All lines in the PROCEDURE DIVISION.
 * @param idx - Current index in the lines array.
 * @param environment - The execution environment.
 * @param outputCallback - Callback to handle output.
 * @returns The updated index after processing the IF statement.
 */
function handleIfStatement(
  lines: string[],
  idx: number,
  environment: Environment,
  outputCallback: (text: string) => void
): number {
  const conditionMatch = lines[idx].match(/IF\s+(.*)/i);
  if (conditionMatch) {
    const condition = conditionMatch[1];
    const conditionResult = evaluateCondition(condition, environment);
    idx++;
    const trueBlock: string[] = [];
    const falseBlock: string[] = [];
    let currentBlock = trueBlock;

    while (idx < lines.length && !lines[idx].startsWith("END-IF")) {
      if (lines[idx].startsWith("ELSE")) {
        currentBlock = falseBlock;
        idx++;
        continue;
      }
      currentBlock.push(lines[idx]);
      idx++;
    }
    if (conditionResult) {
      executeProcedureDivision(trueBlock, environment, outputCallback);
    } else if (falseBlock.length > 0) {
      executeProcedureDivision(falseBlock, environment, outputCallback);
    }
    // Move past END-IF
    if (idx < lines.length && lines[idx].startsWith("END-IF")) {
      idx++;
    } else {
      throw new Error("Missing END-IF for IF statement.");
    }
  } else {
    throw new Error("Invalid IF statement.");
  }
  return idx;
}

/**
 * Handles PERFORM loops, including PERFORM ... TIMES and PERFORM ... UNTIL.
 * @param lines - All lines in the PROCEDURE DIVISION.
 * @param idx - Current index in the lines array.
 * @param environment - The execution environment.
 * @param outputCallback - Callback to handle output.
 * @returns The updated index after processing the PERFORM statement.
 */
function handlePerformLoop(
  lines: string[],
  idx: number,
  environment: Environment,
  outputCallback: (text: string) => void
): number {
  // Handle PERFORM ... TIMES
  const timesMatch = lines[idx].match(/PERFORM\s+(\d+)\s+TIMES/i);
  if (timesMatch) {
    const times = parseInt(timesMatch[1]);
    idx++;
    const loopBody: string[] = [];
    while (idx < lines.length && !lines[idx].startsWith("END-PERFORM")) {
      loopBody.push(lines[idx]);
      idx++;
    }
    for (let i = 0; i < times; i++) {
      executeProcedureDivision(loopBody, environment, outputCallback);
    }
    // Move past END-PERFORM
    if (idx < lines.length && lines[idx].startsWith("END-PERFORM")) {
      idx++;
    } else {
      throw new Error("Missing END-PERFORM for PERFORM loop.");
    }
    return idx;
  }

  // Handle PERFORM ... UNTIL
  const untilMatch = lines[idx].match(/PERFORM\s+UNTIL\s+(.*)/i);
  if (untilMatch) {
    const condition = untilMatch[1];
    idx++;
    const loopBody: string[] = [];
    while (idx < lines.length && !lines[idx].startsWith("END-PERFORM")) {
      loopBody.push(lines[idx]);
      idx++;
    }
    while (!evaluateCondition(condition, environment)) {
      executeProcedureDivision(loopBody, environment, outputCallback);
    }
    // Move past END-PERFORM
    if (idx < lines.length && lines[idx].startsWith("END-PERFORM")) {
      idx++;
    } else {
      throw new Error("Missing END-PERFORM for PERFORM loop.");
    }
    return idx;
  }

  throw new Error("Unsupported PERFORM statement format.");
}

/**
 * Handles EVALUATE statements, functioning similarly to switch-case constructs.
 * @param lines - All lines in the PROCEDURE DIVISION.
 * @param idx - Current index in the lines array.
 * @param environment - The execution environment.
 * @param outputCallback - Callback to handle output.
 * @returns The updated index after processing the EVALUATE statement.
 */
function handleEvaluateStatement(
  lines: string[],
  idx: number,
  environment: Environment,
  outputCallback: (text: string) => void
): number {
  const evaluateExprMatch = lines[idx].match(/EVALUATE\s+(.*)/i);
  if (evaluateExprMatch) {
    const evaluateExpr = evaluateExprMatch[1];
    idx++;
    const cases: { condition: string; body: string[] }[] = [];
    let defaultCase: string[] = [];

    while (idx < lines.length && !lines[idx].startsWith("END-EVALUATE")) {
      if (lines[idx].startsWith("WHEN")) {
        const whenMatch = lines[idx].match(/WHEN\s+(.*)/i);
        if (whenMatch) {
          const condition = whenMatch[1];
          idx++;
          const caseBody: string[] = [];
          while (
            idx < lines.length &&
            !lines[idx].startsWith("WHEN") &&
            !lines[idx].startsWith("END-EVALUATE") &&
            !lines[idx].startsWith("WHEN OTHER")
          ) {
            caseBody.push(lines[idx]);
            idx++;
          }
          cases.push({ condition, body: caseBody });
        } else {
          throw new Error("Invalid WHEN statement.");
        }
      } else if (lines[idx].startsWith("WHEN OTHER")) {
        idx++;
        while (idx < lines.length && !lines[idx].startsWith("END-EVALUATE")) {
          defaultCase.push(lines[idx]);
          idx++;
        }
      } else {
        idx++;
      }
    }

    let matched = false;
    const evaluateValue = evaluateExpression(evaluateExpr, environment);

    for (const { condition, body } of cases) {
      const conditionValue = evaluateExpression(condition, environment);
      if (evaluateValue === conditionValue) {
        executeProcedureDivision(body, environment, outputCallback);
        matched = true;
        break;
      }
    }

    if (!matched && defaultCase.length > 0) {
      executeProcedureDivision(defaultCase, environment, outputCallback);
    }

    // Move past END-EVALUATE
    if (idx < lines.length && lines[idx].startsWith("END-EVALUATE")) {
      idx++;
    } else {
      throw new Error("Missing END-EVALUATE for EVALUATE statement.");
    }
  } else {
    throw new Error("Invalid EVALUATE statement.");
  }
  return idx;
}

/**
 * Handles STRING statements for concatenating multiple variables or literals.
 * @param lines - All lines in the PROCEDURE DIVISION.
 * @param idx - Current index in the lines array.
 * @param environment - The execution environment.
 * @returns The updated index after processing the STRING statement.
 */
function handleStringStatement(
  lines: string[],
  idx: number,
  environment: Environment
): number {
  const stringMatch = lines[idx].match(/STRING\s+(.*)\s+INTO\s+(\w+)/i);
  if (stringMatch) {
    const components = stringMatch[1].split(/\s+DELIMITED BY SIZE\s+/i);
    const targetVar = stringMatch[2];
    let result = "";

    for (const comp of components) {
      const value = evaluateExpression(comp.trim(), environment);
      result += String(value);
    }

    environment.variables[targetVar] = result;
    idx++;
    return idx;
  }
  throw new Error("Invalid STRING statement.");
}

/**
 * Handles UNSTRING statements for splitting a string into multiple variables.
 * @param lines - All lines in the PROCEDURE DIVISION.
 * @param idx - Current index in the lines array.
 * @param environment - The execution environment.
 * @returns The updated index after processing the UNSTRING statement.
 */
function handleUnstringStatement(
  lines: string[],
  idx: number,
  environment: Environment
): number {
  const unstringMatch = lines[idx].match(
    /UNSTRING\s+(\w+)\s+DELIMITED BY SIZE\s+INTO\s+(.*)/i
  );
  if (unstringMatch) {
    const sourceVar = unstringMatch[1];
    const targetVars = unstringMatch[2].split(/\s+/).map((v) => v.trim());
    const sourceValue = String(environment.variables[sourceVar] || "");
    const segmentLength = Math.floor(sourceValue.length / targetVars.length);

    targetVars.forEach((varName, index) => {
      const start = index * segmentLength;
      environment.variables[varName] = sourceValue.substr(start, segmentLength);
    });

    idx++;
    return idx;
  }
  throw new Error("Invalid UNSTRING statement.");
}

/**
 * Evaluates an expression and returns its value.
 * Supports variables, literals, and basic arithmetic operations.
 * @param expr - The expression to evaluate.
 * @param environment - The execution environment.
 * @returns The result of the evaluated expression.
 */
function evaluateExpression(expr: string, environment: Environment): any {
  expr = expr.trim();

  // Handle concatenated expressions separated by space
  if (expr.includes(" ")) {
    const parts = expr
      .split(/\s+/)
      .map((part) => evaluateExpression(part, environment));
    return parts.join(" ");
  }

  // Handle quoted strings
  if (
    (expr.startsWith('"') && expr.endsWith('"')) ||
    (expr.startsWith("'") && expr.endsWith("'"))
  ) {
    return expr.slice(1, -1);
  }

  // Handle numeric values
  if (!isNaN(Number(expr))) {
    return Number(expr);
  }

  // Handle arithmetic expressions (e.g., VAR1 + VAR2)
  const arithmeticMatch = expr.match(/(.+?)([\+\-\*\/])(.+)/);
  if (arithmeticMatch) {
    const left = evaluateExpression(arithmeticMatch[1], environment);
    const operator = arithmeticMatch[2];
    const right = evaluateExpression(arithmeticMatch[3], environment);
    switch (operator) {
      case "+":
        return left + right;
      case "-":
        return left - right;
      case "*":
        return left * right;
      case "/":
        if (right === 0) {
          throw new Error("Division by zero.");
        }
        return left / right;
    }
  }

  // Handle variables
  if (environment.variables[expr] !== undefined) {
    return environment.variables[expr];
  }

  throw new Error(`Undefined variable or invalid expression: ${expr}`);
}

/**
 * Evaluates a condition and returns a boolean result.
 * Supports relational operators and standalone variable evaluations.
 * @param condition - The condition to evaluate.
 * @param environment - The execution environment.
 * @returns True if the condition is met, otherwise false.
 */
function evaluateCondition(
  condition: string,
  environment: Environment
): boolean {
  const operators = ["=", "<>", ">", "<", ">=", "<="];
  let operator = operators.find((op) => condition.includes(` ${op} `));

  if (operator) {
    const [leftExpr, rightExpr] = condition
      .split(` ${operator} `)
      .map((side) => side.trim());
    const left = evaluateExpression(leftExpr, environment);
    const right = evaluateExpression(rightExpr, environment);
    switch (operator) {
      case "=":
        return left === right;
      case "<>":
        return left !== right;
      case ">":
        return left > right;
      case "<":
        return left < right;
      case ">=":
        return left >= right;
      case "<=":
        return left <= right;
    }
  } else {
    // Handle condition without operator (e.g., variable alone)
    const value = evaluateExpression(condition, environment);
    return Boolean(value);
  }
  return false;
}
