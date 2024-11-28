import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";
import { ChevronLeft, ChevronRight } from "lucide-react";

export function LessonContent({ text, keyTakeaways }) {
  return (
    <div className="p-6 space-y-6">
      <div className="prose prose-zinc dark:prose-invert max-w-none">
        <h2>Welcome to COBOL Programming</h2>

        <p>
          COBOL (Common Business-Oriented Language) is a compiled English-like
          computer programming language designed for business use. Let's start
          with understanding the basic structure of a COBOL program.
        </p>

        <h3>Program Structure</h3>

        <p>Every COBOL program consists of four main divisions:</p>

        <ul>
          <li>
            <strong>IDENTIFICATION DIVISION</strong> - Contains program
            identification and metadata
          </li>
          <li>
            <strong>ENVIRONMENT DIVISION</strong> - Describes the computing
            environment
          </li>
          <li>
            <strong>DATA DIVISION</strong> - Defines data items used in the
            program
          </li>
          <li>
            <strong>PROCEDURE DIVISION</strong> - Contains the executable
            program statements
          </li>
        </ul>

        <Card className="p-4 bg-muted/50 not-prose">
          <p className="text-sm font-mono mb-2">Try this example:</p>
          <pre className="text-sm">
            {`       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.`}
          </pre>
        </Card>

        <h3>Key Points</h3>

        <ul>
          <li>
            COBOL statements must begin in column 8 or after (historically
            column 7 was reserved for punch cards)
          </li>
          <li>Each division must end with a period (.)</li>
          <li>
            COBOL is not case-sensitive, but uppercase is traditionally used
          </li>
        </ul>

        <div className="not-prose">
          <p className="text-sm text-muted-foreground mt-4">
            Try modifying the program in the editor to display your own message!
          </p>
        </div>
      </div>

      <div className="flex items-center justify-between pt-4 border-t">
        <Button variant="outline" disabled>
          <ChevronLeft className="mr-2 h-4 w-4" />
          Previous Lesson
        </Button>
        <Button>
          Next Lesson
          <ChevronRight className="ml-2 h-4 w-4" />
        </Button>
      </div>
    </div>
  );
}
