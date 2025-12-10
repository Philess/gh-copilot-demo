import express, { Application, Request, Response } from 'express';
import cors from 'cors';
import albumsRouter from './routes/albums.js';

const app: Application = express();

// Middleware
app.use(cors());
app.use(express.json());

// Root route - welcome message
app.get('/', (_req: Request, res: Response) => {
  res.send('Hit the /albums endpoint to retrieve a list of albums!');
});

// Albums routes
app.use('/albums', albumsRouter);

export default app;
