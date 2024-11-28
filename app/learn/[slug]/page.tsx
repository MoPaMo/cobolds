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

  const keyTakeaways = lesson ? lesson.keyTakeaways : [];
  const text = lesson ? lesson.text : "";
  return (
    <div className="h-screen flex flex-col">
      <header className="border-b bg-card px-6 py-3 flex flex-row">
        <h1 className="text-xl font-semibold">
          Learn COBOL - Lesson {lessonNumber}: Introduction
        </h1>
        <div className="grow"></div>
        <div className="flex flex-row gap-3">
          {slug}
          <Heart />
          <Share />
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
