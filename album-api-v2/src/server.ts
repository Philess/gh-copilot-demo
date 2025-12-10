import express, { Application, Request, Response } from 'express';
import cors from 'cors';
import albumRoutes from './routes/albumRoutes';

const app: Application = express();
const PORT = process.env.PORT || 3000;

// Middleware
app.use(cors());
app.use(express.json());
app.use(express.urlencoded({ extended: true }));

// Routes
app.get('/', (req: Request, res: Response) => {
  res.send('Hit the /albums endpoint to retrieve a list of albums!');
});

app.use('/albums', albumRoutes);

// Start server
app.listen(PORT, () => {
  console.log(`🎵 Album API v2 is running on http://localhost:${PORT}`);
  console.log(`📚 Try: http://localhost:${PORT}/albums`);
});

export default app;
