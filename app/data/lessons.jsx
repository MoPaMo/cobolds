const lessons = [
  {
    id: 1,
    title: "Introduction to COBOL",
    description: "Dive into COBOL and discover why it's still a key player today.",
    duration: "1 hour",
    created_at: "2023-04-10",
    text: (
      <>
        <h1>Introduction to COBOL</h1>
        <p>
          COBOL (Common Business-Oriented Language) is a classic programming language that's been around for decades. It was created mainly for business, finance, and administrative tasks, and it's still powering many legacy systems in large organizations today.
        </p>
        <p>
          In this lesson, we'll journey through the history of COBOL, highlight its standout features, and explore why it remains relevant in today's tech landscape.
        </p>
      </>
    ),
    keyTakeaways: [
      "Understanding the history and evolution of COBOL",
      "Recognizing the areas where COBOL is primarily used",
      "Appreciating the enduring relevance of COBOL in legacy systems",
    ],
    code: `/IDENTIFICATION DIVISION\./,
            /PROGRAM-ID\./`,
    output: (x) => <div>{x}</div>,
  },
  {
    id: 2,
    title: "COBOL Program Structure",
    description: "Discover the basic layout of a COBOL program.",
    duration: "1.5 hours",
    created_at: "2023-04-11",
    text: (
      <>
        <h1>COBOL Program Structure</h1>
        <p>
          A COBOL program is organized into four main sections:
        </p>
        <ol>
          <li>Identification Division</li>
          <li>Environment Division</li>
          <li>Data Division</li>
          <li>Procedure Division</li>
        </ol>
        <p>
          Each section has its own role, handling things like program info, environment settings, data definitions, and the actual code instructions.
        </p>
      </>
    ),
    keyTakeaways: [
      "Identifying the four primary divisions of a COBOL program",
      "Understanding the purpose of each division",
      "Organizing code systematically within a COBOL program",
    ],
    code: `/IDENTIFICATION DIVISION\./,
            /ENVIRONMENT DIVISION\./,
            /DATA DIVISION\./,
            /PROCEDURE DIVISION\./`,
    output: (x) => <div>{x}</div>,
  },
  {
    id: 3,
    title: "Identification Division",
    description: "Take a closer look at the Identification Division.",
    duration: "1 hour",
    created_at: "2023-04-12",
    text: (
      <>
        <h1>Identification Division</h1>
        <p>
          The Identification Division is where you provide key details about your COBOL program, like its name and who wrote it. It's the first section in any COBOL program.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
IDENTIFICATION DIVISION.
PROGRAM-ID. HELLO-WORLD.
AUTHOR. JOHN DOE.
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Defining the Identification Division",
      "Specifying the PROGRAM-ID",
      "Including author and other metadata information",
    ],
    code: `/IDENTIFICATION DIVISION\./,
            /PROGRAM-ID\.\s+\w+\./,
            /AUTHOR\.\s+.+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 4,
    title: "Environment Division",
    description: "Understand the Environment Division and its components.",
    duration: "1 hour",
    created_at: "2023-04-13",
    text: (
      <>
        <h1>Environment Division</h1>
        <p>
          The Environment Division sets up the program's environment, like which devices to use for input and output or how to handle file configurations.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT EMPLOYEE-FILE ASSIGN TO 'EMP.DAT'.
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Defining the Environment Division",
      "Configuring input and output settings",
      "Linking COBOL programs to external files and devices",
    ],
    code: `/ENVIRONMENT DIVISION\./,
            /INPUT-OUTPUT SECTION\./,
            /SELECT\s+\w+\s+ASSIGN TO\s+'.+'\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 5,
    title: "Data Division",
    description: "Learn how to define and manage data in COBOL.",
    duration: "2 hours",
    created_at: "2023-04-14",
    text: (
      <>
        <h1>Data Division</h1>
        <p>
          The Data Division is where you define all the data your program will use. It includes sections like the File Section, Working-Storage Section, and Linkage Section.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
DATA DIVISION.
WORKING-STORAGE SECTION.
01 EMPLOYEE-COUNT PIC 9(4) VALUE ZEROS.
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Defining data items in the Data Division",
      "Understanding different sections within the Data Division",
      "Using PIC clauses to specify data types and sizes",
    ],
    code: `/DATA DIVISION\./,
            /WORKING-STORAGE SECTION\./,
            /01\s+\w+\s+PIC\s+\w+\(\d+\)\.\s+VALUE\s+\w+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 6,
    title: "Procedure Division",
    description: "Dive into the Procedure Division to write executable code.",
    duration: "2 hours",
    created_at: "2023-04-15",
    text: (
      <>
        <h1>Procedure Division</h1>
        <p>
          The Procedure Division is where your COBOL program comes to life with executable instructions. This is where you implement the program's logic using various COBOL statements and operations.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
PROCEDURE DIVISION.
DISPLAY 'Hello, COBOL!'.
STOP RUN.
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Writing executable statements in the Procedure Division",
      "Using DISPLAY and other COBOL statements",
      "Implementing the program's logic",
    ],
    code: `/PROCEDURE DIVISION\./,
            /DISPLAY\s+'.+'\./,
            /STOP RUN\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 7,
    title: "Compiling and Running COBOL Programs",
    description: "Learn how to compile and execute COBOL programs.",
    duration: "1 hour",
    created_at: "2023-04-16",
    text: (
      <>
        <h1>Compiling and Running COBOL Programs</h1>
        <p>
          Once you've written your COBOL code, it's time to compile it. Compiling transforms your source code into an executable form. In this lesson, we'll walk through the basics of compiling and running COBOL programs using popular COBOL compilers.
        </p>
        <p>
          **Steps:**
        </p>
        <ol>
          <li>Write your COBOL code and save it with a `.cob` or `.cbl` extension.</li>
          <li>Use a COBOL compiler to compile your source code.</li>
          <li>Run the compiled program to see the results.</li>
        </ol>
      </>
    ),
    keyTakeaways: [
      "Understanding the compilation process in COBOL",
      "Using COBOL compilers effectively",
      "Executing COBOL programs and interpreting outputs",
    ],
    code: `/DISPLAY\s+'.+'\./,
            /COMPILE\s+\w+\./,
            /RUN\s+\w+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 8,
    title: "Practicing Basic COBOL Syntax",
    description: "Hands-on exercises to practice fundamental COBOL syntax.",
    duration: "2 hours",
    created_at: "2023-04-17",
    text: (
      <>
        <h1>Practicing Basic COBOL Syntax</h1>
        <p>
          Practice makes perfect! In this lesson, you'll engage in exercises that help you write simple COBOL statements and programs, reinforcing your understanding of the syntax.
        </p>
        <p>
          **Exercises:**
        </p>
        <ol>
          <li>Create a COBOL program that displays your name.</li>
          <li>Define a variable to store an employee's ID and assign it a value.</li>
          <li>Write a program that adds two numbers and shows the result.</li>
        </ol>
      </>
    ),
    keyTakeaways: [
      "Applying COBOL syntax in practical scenarios",
      "Writing simple COBOL programs",
      "Enhancing coding proficiency through exercises",
    ],
    code: `/DISPLAY\s+'.+'\./,
            /MOVE\s+\d+\s+TO\s+\w+\./,
            /ADD\s+\d+\s+TO\s+\d+\s+GIVING\s+\w+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 9,
    title: "Working with Variables and Data Types",
    description: "Deepen your understanding of variables and data types in COBOL.",
    duration: "1.5 hours",
    created_at: "2023-04-18",
    text: (
      <>
        <h1>Working with Variables and Data Types</h1>
        <p>
          Variables are the building blocks for storing and manipulating data in your programs. In COBOL, you'll define variables using the PIC clause, which specifies their data type and size.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
01 EMPLOYEE-NAME PIC A(30).
01 EMPLOYEE-ID   PIC 9(5).
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Declaring variables with appropriate data types",
      "Using the PIC clause to define data structure",
      "Understanding alphanumeric and numeric data types",
    ],
    code: `/01\s+\w+\s+PIC\s+A\(\d+\)\./,
            /01\s+\w+\s+PIC\s+9\(\d+\)\./,
            /MOVE\s+\w+\s+TO\s+\w+\./,
            /DISPLAY\s+\w+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 10,
    title: "COBOL Control Structures",
    description: "Learn about loops and conditional statements in COBOL.",
    duration: "2 hours",
    created_at: "2023-04-19",
    text: (
      <>
        <h1>COBOL Control Structures</h1>
        <p>
          Control structures shape the flow of your program. In COBOL, you'll use IF statements for decision-making and PERFORM statements to create loops. These tools help your program respond to different conditions and repeat actions as needed.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
IF EMPLOYEE-ID > 1000
    DISPLAY 'High ID Number'
ELSE
    DISPLAY 'Standard ID Number'.
PERFORM VARYING I FROM 1 BY 1 UNTIL I > 10
    DISPLAY I
END-PERFORM.
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Using IF statements for conditional logic",
      "Implementing loops with PERFORM statements",
      "Controlling program flow in COBOL",
    ],
    code: `/IF\s+.+\s+THEN\./,
            /ELSE\s+DISPLAY\s+'.+'\./,
            /PERFORM\s+VARYING\s+\w+\s+FROM\s+\d+\s+BY\s+\d+\s+UNTIL\s+\w+\s+>/,
            /END-PERFORM\./`,
    output: (x) => <div>{x}</div>,
  },
  {
    id: 11,
    title: "File Handling in COBOL",
    description: "Manage file input and output operations in COBOL.",
    duration: "2 hours",
    created_at: "2023-04-20",
    text: (
      <>
        <h1>File Handling in COBOL</h1>
        <p>
          COBOL excels at handling files. You can set up files in the Environment Division and perform actions like reading from and writing to them in the Procedure Division.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT EMPLOYEE-FILE ASSIGN TO 'EMP.DAT'.

DATA DIVISION.
FILE SECTION.
FD EMPLOYEE-FILE.
01 EMPLOYEE-RECORD.
   05 EMPLOYEE-ID   PIC 9(5).
   05 EMPLOYEE-NAME PIC A(30).

PROCEDURE DIVISION.
OPEN INPUT EMPLOYEE-FILE.
READ EMPLOYEE-FILE INTO EMPLOYEE-RECORD.
DISPLAY EMPLOYEE-ID, EMPLOYEE-NAME.
CLOSE EMPLOYEE-FILE.
STOP RUN.
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Defining and associating external files",
      "Opening, reading, and closing files",
      "Handling file I/O operations in COBOL",
    ],
    code: `/OPEN\s+INPUT\s+\w+\./,
            /READ\s+\w+\s+INTO\s+\w+\./,
            /DISPLAY\s+\w+,\s+\w+\./,
            /CLOSE\s+\w+\./`,
    output: (x) => <code>{x}</code>,
  },
  {
    id: 12,
    title: "Practicing File Operations in COBOL",
    description: "Hands-on exercises to practice file input and output in COBOL.",
    duration: "2.5 hours",
    created_at: "2023-04-21",
    text: (
      <>
        <h1>Practicing File Operations in COBOL</h1>
        <p>
          Strengthen your file handling skills with practical exercises. These tasks will help you become adept at managing files within your COBOL programs.
        </p>
        <p>
          **Exercises:**
        </p>
        <ol>
          <li>Create a COBOL program that reads employee data from a file and displays it.</li>
          <li>Write a program that adds new employee records to an existing file.</li>
          <li>Develop a program that updates specific records in a file based on certain criteria.</li>
        </ol>
      </>
    ),
    keyTakeaways: [
      "Implementing file read and write operations",
      "Handling multiple records within a file",
      "Developing programs that interact with external data sources",
    ],
    code: `/OPEN\s+INPUT\s+\w+\./,
            /READ\s+\w+\s+INTO\s+\w+\./,
            /DISPLAY\s+\w+\./,
            /CLOSE\s+\w+\./,
            /OPEN\s+OUTPUT\s+\w+\./,
            /WRITE\s+\w+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 13,
    title: "COBOL Sorting and Merging",
    description: "Learn how to sort and merge data in COBOL programs.",
    duration: "2 hours",
    created_at: "2023-04-22",
    text: (
      <>
        <h1>COBOL Sorting and Merging</h1>
        <p>
          Organizing data efficiently is crucial. COBOL offers SORT and MERGE commands to help you arrange and combine data sets seamlessly.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
SORT EMPLOYEE-FILE
    ON ASCENDING KEY EMPLOYEE-ID
    USING INPUT-FILE
    GIVING SORTED-FILE.
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Using the SORT verb to order data",
      "Merging multiple files or data sets",
      "Optimizing data processing through sorting and merging",
    ],
    code: `/SORT\s+\w+\s+ON\s+\w+\s+KEY\s+\w+\./,
            /USING\s+\w+\./,
            /GIVING\s+\w+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 14,
    title: "Practicing Sorting and Merging in COBOL",
    description: "Hands-on exercises to practice sorting and merging data in COBOL.",
    duration: "2.5 hours",
    created_at: "2023-04-23",
    text: (
      <>
        <h1>Practicing Sorting and Merging in COBOL</h1>
        <p>
          Put your sorting and merging skills to the test with these practical exercises. You'll learn how to manage and organize data effectively in your COBOL programs.
        </p>
        <p>
          **Exercises:**
        </p>
        <ol>
          <li>Write a COBOL program that sorts employee records by their ID.</li>
          <li>Create a program that merges two sorted employee files into one.</li>
          <li>Develop a program that sorts employee data by name in descending order.</li>
        </ol>
      </>
    ),
    keyTakeaways: [
      "Implementing data sorting based on specific keys",
      "Merging multiple sorted files efficiently",
      "Customizing sort orders to meet program requirements",
    ],
    code: `/SORT\s+\w+\s+ON\s+\w+\s+KEY\s+\w+\./,
            /USING\s+\w+\./,
            /GIVING\s+\w+\./`,
    output: (x) => <pre>{x}</pre>,
  },
  {
    id: 15,
    title: "COBOL Arrays and Tables",
    description: "Manage collections of data using arrays and tables in COBOL.",
    duration: "2 hours",
    created_at: "2023-04-24",
    text: (
      <>
        <h1>COBOL Arrays and Tables</h1>
        <p>
          Handling multiple data items is easier with arrays and tables. COBOL lets you create these structures to manage collections of similar data efficiently.
        </p>
        <p>
          **Example:**
        </p>
        <pre>
          {`
01 EMPLOYEE-TABLE.
   05 EMPLOYEE-RECORD OCCURS 100 TIMES.
      10 EMPLOYEE-ID   PIC 9(5).
      10 EMPLOYEE-NAME PIC A(30).
          `}
        </pre>
      </>
    ),
    keyTakeaways: [
      "Defining arrays and tables in COBOL",
      "Accessing and manipulating array elements",
      "Iterating over tables with nested loops",
    ],
    code: `/01\s+\w+\s+TABLE\s+OF\s+\w+-RECORD,\s+SIZE\s+\d+\./,
            /MOVE\s+\w+\s+TO\s+\w+\(\d+\)\./,
            /PERFORM\s+VARYING\s+\w+\s+FROM\s+\d+\s+BY\s+\d+\s+UNTIL\s+\w+\s+>/`,
    output: (x) => <pre>{x}</pre>,
  }

]

export default lessons;