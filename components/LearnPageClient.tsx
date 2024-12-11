// components/LearnPageClient.tsx
"use client";

import { CodeEditor } from "@/components/code-editor";
import { LessonContent } from "@/components/lesson-content";
import { Button } from "@/components/ui/button";
import {
  ResizableHandle,
  ResizablePanel,
  ResizablePanelGroup,
} from "@/components/ui/resizable";
import { Terminal } from "@/components/terminal";
import { useState, useEffect } from "react";
import { Share, Heart, HeartOff, Home } from "lucide-react";
import Link from "next/link";
import { notFound } from "next/navigation";
import { Lesson } from "@/app/data/lessons"; // Adjust the import based on your actual lesson type

interface LearnPageClientProps {
  lesson: Lesson;
  lessons: Lesson[];
}

const LearnPageClient: React.FC<LearnPageClientProps> = ({ lesson, lessons }) => {
  const [liked, setLiked] = useState(false);
  const [keyTakeaways, setKeyTakeaways] = useState<string[]>([]);
  const [text, setText] = useState(<></>);

  // New states for the emulator
  const [userCode, setUserCode] = useState<string>("");
  const [terminalOutput, setTerminalOutput] = useState<string[]>([]);

  useEffect(() => {
    // Ensure window is available
    if (typeof window !== "undefined") {
      // Set current lesson to local storage
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
      if (typeof lesson.output === 'string') {
        output.push(lesson.output);
      } else {
        console.error('lesson.output is not a string');
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
    <div className="h-screen flex flex-col">
      <header className="border-b bg-card px-6 py-3 flex flex-row">
        <Link href="/">
          <Home />{" "}
        </Link>

        <h1 className="text-xl font-semibold">
          Learn COBOL - Lesson {lesson.id}: {lesson.title}
        </h1>
        <div className="grow"></div>
        <div className="flex flex-row gap-3">
          <Button
            variant="secondary"
            onClick={() => {
              setLiked((prev) => !prev);
            }}
          >
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
            <div className="h-full overflow-y-auto">
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
    </div>
  );
};

export default LearnPageClient;