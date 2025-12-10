import app from './app.js';

const PORT = process.env.PORT || 3000;

app.listen(PORT, () => {
  console.log(`🎵 Album API v2 is running on http://localhost:${PORT}`);
  console.log(`📀 Hit /albums to retrieve a list of albums`);
});
