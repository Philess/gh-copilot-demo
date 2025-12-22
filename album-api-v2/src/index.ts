import app from './app';

const port = Number(process.env.PORT || 3000);

app.listen(port, () => {
  // eslint-disable-next-line no-console
  console.log(`album-api-v2 listening on http://localhost:${port}`);
});
