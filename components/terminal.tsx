"use client"

export function Terminal() {
  return (
    <div className="h-full flex flex-col bg-zinc-950 text-zinc-50">
      <div className="flex items-center px-4 py-2 border-b border-zinc-800 bg-zinc-900">
        <div className="text-sm font-medium">Output</div>
      </div>
      <div className="flex-1 p-4 font-mono text-sm overflow-auto">
        <div className="text-emerald-400">$ Running COBOL program...</div>
        <div className="mt-2">Hello, World!</div>
        <div className="text-emerald-400 mt-2">$ Program completed with return code 0</div>
      </div>
    </div>
  )
}

