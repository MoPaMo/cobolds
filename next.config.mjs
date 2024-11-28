/** @type {import('next').NextConfig} */
const nextConfig = {
  async redirects() {
    return [
      {
        source: "/learn",
        destination: "/learn/introduction-1",
        permanent: true,
      },
    ];
  },
};

export default nextConfig;
