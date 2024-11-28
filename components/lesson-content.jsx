import { Button } from "@/components/ui/button";
import {
  Card,
  CardContent,
  CardDescription,
  CardFooter,
  CardHeader,
  CardTitle,
} from "@/components/ui/card";
import { ChevronLeft, ChevronRight } from "lucide-react";

export function LessonContent({ keyTakeaways, children }) {
  return (
    <div className="p-6 space-y-6">
      <div className="prose prose-zinc dark:prose-invert max-w-none">
        {" "}
        {children}
      </div>
      <Card>
        <CardHeader>
          <CardTitle>Key Takeaways</CardTitle>
          <CardDescription>
            If you remember anything from this lesson, remember these points :)
          </CardDescription>
        </CardHeader>
        <CardContent>
          <ul className="list-disc list-inside">
            {keyTakeaways.map((keyTakeaway, index) => (
              <li key={index}>{keyTakeaway}</li>
            ))}
          </ul>
        </CardContent>
      </Card>

      <div className="flex items-center justify-between pt-4 border-t">
        <Button variant="outline" disabled>
          <ChevronLeft className="mr-2 h-4 w-4" />
          Previous Lesson
        </Button>
        <Button>
          Next Lesson
          <ChevronRight className="ml-2 h-4 w-4" />
        </Button>
      </div>
    </div>
  );
}
