"use client";
import { CodeEditor } from "@/components/code-editor";
import { LessonContent } from "@/components/lesson-content";
import {
  ResizableHandle,
  ResizablePanel,
  ResizablePanelGroup,
} from "@/components/ui/resizable";
import { Card } from "@/components/ui/card";
import { Terminal } from "@/components/terminal";
import { useState } from "react";
import { Share, Heart } from "lucide-react";

export default function LearnPage() {
  const [text, setText] = useState(
    `COBOL (Common Business-Oriented Language) is a high-level programming language used primarily for business, finance, and administrative systems. It was first introduced in 1959 by the CODASYL (Conference on Data Systems Languages) committee, which was responsible for developing a standard programming language for business data processing.)`
  );
  const [keyTakeaways, setKeyTakeaways] = useState([
    `COBOL (Common Business-Oriented Language) is a high-level programming language used primarily for business, finance, and administrative systems. It was first introduced in 1959 by the CODASYL (Conference on Data Systems Languages) committee, which was responsible for developing a standard programming language for business data processing.)`,
  ]);
  return (
    <div className="h-screen flex flex-col">
      <header className="border-b bg-card px-6 py-3 flex flex-row">
        <h1 className="text-xl font-semibold">
          Learn COBOL - Lesson 1: Introduction
        </h1>
        <div className="grow"></div>
        <div className="flex flex-row gap-3">
          <Heart></Heart>
          <Share></Share>
        </div>
      </header>

      <main className="flex-1 overflow-hidden">
        <ResizablePanelGroup direction="horizontal">
          {/* Lesson Content Panel */}
          <ResizablePanel defaultSize={40}>
            <div className="h-full overflow-y-auto">
              <LessonContent keyTakeaways={keyTakeaways}>{text}</LessonContent>
            </div>
          </ResizablePanel>

          <ResizableHandle />

          {/* Code Editor and Terminal Panel */}
          <ResizablePanel defaultSize={60}>
            <ResizablePanelGroup direction="vertical">
              <ResizablePanel defaultSize={60}>
                <CodeEditor />
              </ResizablePanel>

              <ResizableHandle />

              <ResizablePanel defaultSize={40}>
                <Terminal />
              </ResizablePanel>
            </ResizablePanelGroup>
          </ResizablePanel>
        </ResizablePanelGroup>
      </main>
    </div>
  );
}
