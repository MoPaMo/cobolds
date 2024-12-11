import LearnPageClient from "@/components/LearnPageClient";
import lessons from "@/app/data/lessons";
import { notFound } from "next/navigation";

interface LearnPageParams {
  slug: string;
}

export async function generateStaticParams() {
  return lessons.map((lesson) => ({
    slug: `lesson-${lesson.id}`, // Adjust based on your slug format
  }));
}

const LearnPage = ({ params }: { params: LearnPageParams }) => {
  const slug = params.slug;
  const lessonNumber = Number(slug.split("-").pop());

  const lesson = lessons.find((lesson) => lesson.id === lessonNumber);
  if (!lesson) {
    // Raise 404 error
    notFound();
  }

  return <LearnPageClient lesson={lesson} lessons={lessons} />;
};

export default LearnPage;