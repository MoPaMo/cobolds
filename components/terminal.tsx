// cobolds/components/terminal.tsx
"use client";

interface TerminalProps {
  output: string[];
}

export function Terminal({ output }: TerminalProps) {
  return (
    <div className="h-full flex flex-col bg-zinc-950 text-zinc-50">
      <div className="flex items-center px-4 py-2 border-b border-zinc-800 bg-zinc-900">
        <div className="flex items-center gap-2">
          <div className="w-3 h-3 rounded-full bg-red-500" />
          <div className="w-3 h-3 rounded-full bg-yellow-500" />
          <div className="w-3 h-3 rounded-full bg-green-500" />
          <div className="text-sm font-medium">Output</div>
        </div>
      </div>
      <div className="flex-1 p-4 font-mono text-sm overflow-auto">
        {output.map((line, index) => (
          <div key={index}>{line}</div>
        ))}
      </div>
    </div>
  );
}
