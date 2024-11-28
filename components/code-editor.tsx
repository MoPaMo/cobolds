"use client";

import { useState } from "react";
import { Button } from "@/components/ui/button";
import { Play } from "lucide-react";

export function CodeEditor() {
  const [code, setCode] = useState(`       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, World!".
           STOP RUN.`);

  return (
    <div className="h-full flex flex-col">
      <div className="flex items-center justify-between px-4 py-2 border-b bg-muted/50">
        <div className="flex items-center gap-2  ">
          <div className="w-3 h-3 rounded-full bg-red-500" />
          <div className="w-3 h-3 rounded-full bg-yellow-500" />
          <div className="w-3 h-3 rounded-full bg-green-500" />
          <span className="ml-2 text-zinc-500 text-sm">main.cbl</span>
        </div>
        <Button size="sm" variant="default">
          <Play className="h-4 w-4 mr-2" />
          Run
        </Button>
      </div>
      <div className="flex-1 p-4 font-mono text-sm overflow-auto bg-background">
        <textarea
          value={code}
          onChange={(e) => setCode(e.target.value)}
          className="w-full h-full resize-none bg-transparent border-0 focus:outline-none"
          spellCheck="false"
          aria-label="COBOL code editor"
        />
      </div>
    </div>
  );
}
