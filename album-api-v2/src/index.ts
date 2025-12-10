import express, { Application, Request, Response } from 'express';
import cors from 'cors';
import albumRoutes from './routes/albums';

const app: Application = express();
const PORT = process.env.PORT || 3000;

// Middleware
app.use(cors());
app.use(express.json());

// Routes
app.use('/albums', albumRoutes);

// Health check
app.get('/', (req: Request, res: Response) => {
  res.json({ message: 'Album API v2 is running' });
});

// Start server only if not in test environment
if (process.env.NODE_ENV !== 'test') {
  app.listen(PORT, () => {
    console.log(`🚀 Server is running on http://localhost:${PORT}`);
  });
}

export default app;
