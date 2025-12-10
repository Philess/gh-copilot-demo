import express from 'express';
import cors from 'cors';
import albumsRouter from './routes/albums';

const app = express();
app.use(cors());
app.use(express.json());
app.use('/', albumsRouter);

const port = process.env.PORT ? Number(process.env.PORT) : 3000;
app.listen(port, () => {
  console.log(`Album API v2 listening on port ${port}`);
});

export default app;
