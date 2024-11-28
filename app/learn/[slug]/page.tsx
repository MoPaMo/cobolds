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
import { use, useState, useEffect } from "react";
import { Share, Heart } from "lucide-react";

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

  const lesson = lessons.find((lesson) => lesson.id == lessonNumber);
  if (!lesson) {
    // raise 404 error
    return notFound();
  }
  // set current lesson to local storage
  localStorage.setItem("currentLesson", JSON.stringify(lesson.id));
  const lessonLikes = JSON.parse(localStorage.getItem("lessonLikes") || "[]");

  const [liked, setLiked] = useState(lessonLikes.includes(lesson.id));
  useEffect(() => {
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
    localStorage.setItem("lessonLikes", JSON.stringify(lessonLikes));
  }, [liked, lesson.id, lessonLikes]);

  const keyTakeaways = lesson ? lesson.keyTakeaways : [];
  const text = lesson ? lesson.text : "";
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
              setLiked((l: any) => {
                return !l;
              });
            }}
          >
            {liked ? <Heart fill="true"></Heart> : <Heart />}
          </Button>
          <Button
            variant="secondary"
            onClick={() => {
              navigator.share({
                title:
                  "Learn COBOL - Lesson " + lesson.id + ": " + lesson.title,
                text:
                  "Check out this COBOL lesson on " + window.location.hostname,
                url: window.location.href,
              });
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
                nextLesson={lessons[lessonNumber + 1]}
                prevLesson={lessons[lessonNumber - 1]}
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
