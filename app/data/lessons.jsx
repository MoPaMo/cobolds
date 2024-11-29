const lessons = [
    {
      id: 1,
      title: "Introduction to COBOL",
      description: "Get acquainted with COBOL and its historical significance.",
      text: (
        <>
          <h1>Introduction to COBOL</h1>
          <p>
            COBOL (Common Business-Oriented Language) is one of the oldest programming languages,
            designed primarily for business, finance, and administrative systems. Despite its age,
            COBOL remains in use today, especially in legacy systems within large organizations.
          </p>
          <p>
            In this lesson, we'll explore the origins of COBOL, its key features, and why it continues
            to be relevant in the modern programming landscape.
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
      description: "Learn the basic structure of a COBOL program.",
      text: (
        <>
          <h1>COBOL Program Structure</h1>
          <p>
            A COBOL program is divided into four main divisions:
          </p>
          <ol>
            <li>Identification Division</li>
            <li>Environment Division</li>
            <li>Data Division</li>
            <li>Procedure Division</li>
          </ol>
          <p>
            Each division serves a specific purpose, organizing the program's metadata, environment settings,
            data definitions, and executable instructions.
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
      description: "Explore the Identification Division in detail.",
      text: (
        <>
          <h1>Identification Division</h1>
          <p>
            The Identification Division provides metadata about the COBOL program, such as its name and author.
            It is the first division in a COBOL program.
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
    }
]

export default lessons;