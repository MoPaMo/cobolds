import { CodeEditor } from "@/components/code-editor"
import { LessonContent } from "@/components/lesson-content"
import { ResizableHandle, ResizablePanel, ResizablePanelGroup } from "@/components/ui/resizable"
import { Terminal } from "@/components/terminal"

export default function LearnPage() {
  return (
    <div className="h-screen flex flex-col">
      <header className="border-b bg-card px-6 py-3">
        <h1 className="text-xl font-semibold">Learn COBOL - Lesson 1: Introduction</h1>
      </header>
      
      <main className="flex-1 overflow-hidden">
        <ResizablePanelGroup direction="horizontal">
          {/* Lesson Content Panel */}
          <ResizablePanel defaultSize={40}>
            <div className="h-full overflow-y-auto">
              <LessonContent />
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
  )
}

