import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

const albumApiHost = process.env.VITE_ALBUM_API_HOST ?? 'localhost:3000'

export default defineConfig({
  plugins: [react()],
  server: {
    port: 3001,
    proxy: {
      '/albums': {
        target: `http://${albumApiHost}`,
        changeOrigin: true,
      },
    },
  },
})
