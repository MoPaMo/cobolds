const lessons = [
  {
    id: 1,
    title: "Introduction to React",
    description: "In this lesson, we will learn about the basics of React.",
    duration: "1 hour",
    created_at: "2021-01-01",
    text: (
      <>
        <h1> Intro</h1>
      </>
    ),
    keyTakeaways: [
      "Understanding the purpose of React",
      "Setting up a React project",
      "Creating a simple React component",
    ],
    code: `const element = <h1>Hello, world!</h1>;`,
    output: `<h1>Hello, world!</h1>`,
  },
  {
    id: 2,
    title: "Lesson 2: React Components",
    description: "In this lesson, we will learn about React components.",
    duration: "1.5 hours",
    created_at: "2021-01-02",
    text: (
      <>
        <h1> Intro</h1>
      </>
    ),
    keyTakeaways: [
      "Understanding the component lifecycle",
      "Creating functional and class components",
      "Passing props to components",
    ],
    code: `function Welcome(props) {
    return <h1>Hello, {props.name}</h1>;
}`,
    output: `<h1>Hello, Sara</h1>`,
  },
  {
    id: 3,
    title: "Lesson 3: React Hooks",
    description: "In this lesson, we will learn about React hooks.",
    duration: "2 hours",
    created_at: "2021-01-03",
    text: (
      <>
        <h1> Intro</h1>
      </>
    ),
    keyTakeaways: [
      "Using the useState hook",
      "Using the useEffect hook",
      "Building custom hooks",
    ],
    code: `const [count, setCount] = useState(0);`,
    output: `0`,
  },
];
export default lessons;
