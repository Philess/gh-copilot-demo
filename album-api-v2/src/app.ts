import express from 'express';
import cors from 'cors';
import albumsRouter from './routes/albums';

const app = express();
app.use(cors());
app.use(express.json());

app.use('/albums', albumsRouter);

app.get('/', (req, res) => res.send({ status: 'album-api-v2', ok: true }));

export default app;
