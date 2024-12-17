// cobolds/app/learn/[slug]/LearnPageContent.tsx

"use client";

import { useState, useEffect } from "react";
import { Button } from "@/components/ui/button";
import { Share, Heart, HeartOff, Home } from "lucide-react";
import Link from "next/link";
import { CodeEditor } from "@/components/code-editor";
import { Terminal } from "@/components/terminal";
import {
  ResizableHandle,
  ResizablePanel,
  ResizablePanelGroup,
} from "@/components/ui/resizable";
import { LessonContent } from "@/components/lesson-content";

import lessons from "@/app/data/lessons";

type Lesson = {
  id: number;
  title: string;
  description: string;
  duration: string;
  created_at: string;
  text: React.ReactNode;
  keyTakeaways: string[];
  code: RegExp[];
  output: (x: any) => React.ReactNode;
};

type Props = {
  lesson: Lesson;
};

export default function LearnPageContent({ lesson }: Props) {
  const [liked, setLiked] = useState(false);
  const [keyTakeaways, setKeyTakeaways] = useState<string[]>([]);
  const [text, setText] = useState(<></>);

  // New states for the emulator
  const [userCode, setUserCode] = useState<string>("");
  const [terminalOutput, setTerminalOutput] = useState<string[]>([]);

  useEffect(() => {
    // Set current lesson to local storage
    if (typeof window !== "undefined") {
      window.localStorage.setItem("currentLesson", JSON.stringify(lesson.id));

      // Retrieve lesson likes
      const storedLikes = window.localStorage.getItem("lessonLikes");
      const lessonLikes = storedLikes ? JSON.parse(storedLikes) : [];

      // Initialize liked state
      setLiked(lessonLikes.includes(lesson.id));

      // Set key takeaways and text
      setKeyTakeaways(lesson.keyTakeaways || []);
      setText(lesson.text);
    }
  }, [lesson]);

  useEffect(() => {
    if (typeof window !== "undefined") {
      const storedLikes = window.localStorage.getItem("lessonLikes");
      const lessonLikes: number[] = storedLikes ? JSON.parse(storedLikes) : [];

      if (liked) {
        if (!lessonLikes.includes(lesson.id)) {
          lessonLikes.push(lesson.id);
        }
      } else {
        const index = lessonLikes.indexOf(lesson.id);
        if (index > -1) {
          lessonLikes.splice(index, 1);
        }
      }
      window.localStorage.setItem("lessonLikes", JSON.stringify(lessonLikes));
    }
  }, [liked, lesson.id]);

  // Emulator "Run" handler
  const handleRun = () => {
    const output: string[] = [];
    output.push(`$ Running COBOL program...`);

    if (
      Array.isArray(lesson.code) &&
      lesson.code.every((pattern: RegExp) => pattern.test(userCode))
    ) {
      try {
        const result = lesson.output(userCode);
        if (typeof result === "string") {
          output.push(result);
        } else if (React.isValidElement(result)) {
          // Convert React elements to strings or handle appropriately
          output.push("Program executed successfully.");
        }
      } catch (error) {
        console.error("Error executing lesson.output:", error);
        output.push("Error executing program.");
      }
      output.push(`$ Program completed with return code 0`);
    } else {
      output.push(
        `% Error: Your COBOL program has syntax errors or is incomplete. Please review the lesson guidelines and try again.`
      );
    }

    setTerminalOutput(output);
  };

  return (
    <>
      <header className="flex items-center justify-between p-4 bg-card border-b">
        <Link href="/">
          <Home />{" "}
        </Link>

        <h1 className="text-xl font-semibold">
          Learn COBOL - Lesson {lesson.id}: {lesson.title}
        </h1>
        <div className="flex space-x-3">
          <Button variant="secondary" onClick={() => setLiked((prev) => !prev)}>
            {liked ? <HeartOff /> : <Heart />}
          </Button>
          <Button
            variant="secondary"
            onClick={() => {
              if (navigator.share) {
                navigator
                  .share({
                    title:
                      "Learn COBOL - Lesson " + lesson.id + ": " + lesson.title,
                    text:
                      "Check out this COBOL lesson on " +
                      window.location.hostname,
                    url: window.location.href,
                  })
                  .catch((error) => {
                    console.error("Error sharing:", error);
                  });
              } else {
                // Fallback for browsers that do not support the Web Share API
                alert("Share not supported on this browser.");
              }
            }}
          >
            <Share />
          </Button>
        </div>
      </header>

      <main className="flex-1 overflow-hidden">
        <ResizablePanelGroup direction="horizontal">
          {/* Lesson Content Panel */}
          <ResizablePanel defaultSize={40}>
            <div className="h-full overflow-y-auto p-4">
              <LessonContent
                keyTakeaways={keyTakeaways}
                nextLesson={lessons[lesson.id]}
                prevLesson={lessons[lesson.id - 2]}
              >
                {text}
              </LessonContent>
            </div>
          </ResizablePanel>

          <ResizableHandle />

          {/* Code Editor and Terminal Panel */}
          <ResizablePanel defaultSize={60}>
            <ResizablePanelGroup direction="vertical">
              <ResizablePanel defaultSize={60}>
                {/* Code Editor */}
                <CodeEditor
                  code={userCode}
                  onCodeChange={setUserCode}
                  onRun={handleRun}
                />
              </ResizablePanel>

              <ResizableHandle />

              <ResizablePanel defaultSize={40}>
                {/* Terminal */}
                <Terminal output={terminalOutput} />
              </ResizablePanel>
            </ResizablePanelGroup>
          </ResizablePanel>
        </ResizablePanelGroup>
      </main>
    </>
  );
}
