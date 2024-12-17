// cobolds/app/learn/[slug]/page.tsx

import lessonsData from "@/app/data/lessons";
import LearnPageContent from "./LearnPageContent"; // Import the Client Component

type Params = {
  slug: string;
};

// Server Component
export default function LearnPage({ params }: { params: Params }) {
  const slug = params.slug;
  const lessonNumber = Number(slug.split("-").pop());

  const lesson = lessonsData.find((lesson) => lesson.id === lessonNumber);
  if (!lesson) {
    // Raise 404 error
    return notFound();
  }

  return <LearnPageContent lesson={lesson} />;
}

// Export the generateStaticParams function
export async function generateStaticParams(): Promise<Params[]> {
  return lessonsData.map((lesson) => ({
    slug: `lesson-${lesson.id}`,
  }));
}
