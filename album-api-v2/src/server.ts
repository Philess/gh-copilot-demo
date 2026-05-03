import { createApp } from './app.js';

const PORT = 3000;

const app = createApp();

app.listen(PORT, () => {
  console.log(`🎵 Album API v2 is running on http://localhost:${PORT}`);
  console.log(`📚 Hit http://localhost:${PORT}/albums to retrieve a list of albums!`);
});
