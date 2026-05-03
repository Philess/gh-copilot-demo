import express, { Request, Response } from 'express';
import cors from 'cors';
import { albumRoutes } from './routes/albumRoutes.js';

/**
 * Create and configure the Express application
 */
export function createApp() {
  const app = express();

  // Middleware
  app.use(cors()); // Enable CORS for all origins (matching .NET API configuration)
  app.use(express.json()); // Parse JSON request bodies

  // Root endpoint
  app.get('/', (req: Request, res: Response) => {
    res.send('Hit the /albums endpoint to retrieve a list of albums!');
  });

  // Album routes
  app.use('/albums', albumRoutes);

  return app;
}
