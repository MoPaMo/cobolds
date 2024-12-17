import Link from "next/link";
import { Terminal } from "lucide-react";
import { Button } from "@/components/ui/button";
import { Card } from "@/components/ui/card";

export default function Page() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-zinc-900 to-black text-white">
      {/* Hero Section */}
      <div className="container mx-auto px-4 pt-20 pb-16">
        <div className="text-center space-y-4">
          <h1 className="text-4xl md:text-6xl font-bold bg-gradient-to-r from-green-400 to-emerald-600 text-transparent bg-clip-text">
            Learn COBOL Interactively
          </h1>
          <p className="text-xl text-zinc-400 max-w-2xl mx-auto">
            Master the language that powers 70% of business transactions
            worldwide. Interactive lessons, real-world examples, and instant
            feedback.
          </p>
        </div>

        {/* Interactive Terminal */}
        <Card className="mt-12 p-4 bg-zinc-900 border-zinc-800 max-w-3xl mx-auto">
          <div className="flex items-center gap-2 mb-4">
            <div className="w-3 h-3 rounded-full bg-red-500" />
            <div className="w-3 h-3 rounded-full bg-yellow-500" />
            <div className="w-3 h-3 rounded-full bg-green-500" />
            <span className="ml-2 text-sm text-zinc-500">
              cobol-example.cbl
            </span>
          </div>
          <pre className="font-mono text-sm md:text-base bg-black rounded-lg p-4 overflow-x-auto shadow-lg">
            <code className="text-green-400 ">
              {`       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO-WORLD.
       
       PROCEDURE DIVISION.
           DISPLAY "Hello, COBOL learner!".
           STOP RUN.`}
            </code>
          </pre>
        </Card>

        {/* Features Grid */}
        <div className="grid md:grid-cols-3 gap-8 mt-20">
          <div className="space-y-3">
            <h3 className="text-xl font-semibold text-emerald-400">
              Interactive Learning
            </h3>
            <p className="text-zinc-400">
              Write and execute COBOL code directly in your browser. Get instant
              feedback and learn by doing.
            </p>
          </div>
          <div className="space-y-3">
            <h3 className="text-xl font-semibold text-emerald-400">
              Real-World Focus
            </h3>
            <p className="text-zinc-400">
              Learn with practical examples from real world scenarios.
            </p>
          </div>
          <div className="space-y-3">
            <h3 className="text-xl font-semibold text-emerald-400">
              Step-by-Step Progress
            </h3>
            <p className="text-zinc-400">
              Start from basics and progress to advanced COBOL concepts at your
              own pace.
            </p>
          </div>
        </div>

        {/* CTA Section */}
        <div className="mt-20 text-center">
          <Link href="/learn/introduction-1">
            <Button size="lg" className="bg-emerald-600 hover:bg-emerald-700">
              <Terminal className="mr-2 h-5 w-5" />
              Start Learning COBOL
            </Button>
          </Link>
          <p className="mt-4 text-zinc-500">
            No installation required. Start coding right away.
          </p>
        </div>

        {/* Stats Section */}
        <div className="mt-20 grid grid-cols-2 md:grid-cols-4 gap-8">
          <div className="text-center">
            <div className="text-3xl font-bold text-emerald-400">70%</div>
            <div className="text-zinc-400 text-sm mt-1">
              of Business Transactions
            </div>
          </div>
          <div className="text-center">
            <div className="text-3xl font-bold text-emerald-400">220B+</div>
            <div className="text-zinc-400 text-sm mt-1">Lines of Code</div>
          </div>
          <div className="text-center">
            <div className="text-3xl font-bold text-emerald-400">$3T+</div>
            <div className="text-zinc-400 text-sm mt-1">Daily Transactions</div>
          </div>
          <div className="text-center">
            <div className="text-3xl font-bold text-emerald-400">65+</div>
            <div className="text-zinc-400 text-sm mt-1">Years of Legacy</div>
          </div>
        </div>
      </div>

      {/* Footer */}
      <footer className="mt-20 border-t border-zinc-800 py-8">
        <div className="container mx-auto px-4 text-center text-zinc-500">
          <p>
            © {new Date().getFullYear()} cobols. Learn COBOL for the modern
            world.
          </p>
        </div>
      </footer>
    </div>
  );
}
