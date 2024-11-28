"use client";

import { CodeEditor } from "@/components/code-editor";
import { LessonContent } from "@/components/lesson-content";
import { Button } from "@/components/ui/button";
import {
  ResizableHandle,
  ResizablePanel,
  ResizablePanelGroup,
} from "@/components/ui/resizable";
import { Card } from "@/components/ui/card";
import { Terminal } from "@/components/terminal";
import { useState, useEffect } from "react"; // Removed unused 'use'
import { Share, Heart, HeartOff } from "lucide-react";

import { useParams } from "next/navigation";
import { notFound } from "next/navigation";

import lessons from "@/app/data/lessons";

export default function LearnPage() {
  const params = useParams();
  if (!params) {
    return <div>Error: No parameters found</div>;
  }
  const slug = params.slug as string;
  const lessonNumber = Number(slug.split("-").pop());

  const lesson = lessons.find((lesson) => lesson.id === lessonNumber);
  if (!lesson) {
    // Raise 404 error
    return notFound();
  }

  const [liked, setLiked] = useState(false);
  const [keyTakeaways, setKeyTakeaways] = useState<string[]>([]);
  const [text, setText] = useState(<></>);

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

  return (
    <div className="h-screen flex flex-col">
      <header className="border-b bg-card px-6 py-3 flex flex-row">
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
                nextLesson={lessons[lessonNumber]}
                prevLesson={lessons[lessonNumber - 2]}
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
                <CodeEditor text={lesson.code} />
              </ResizablePanel>

              <ResizableHandle />

              <ResizablePanel defaultSize={40}>
                <Terminal text={lesson.out} />
              </ResizablePanel>
            </ResizablePanelGroup>
          </ResizablePanel>
        </ResizablePanelGroup>
      </main>
    </div>
  );
}
