import express, { Application } from 'express';
import cors from 'cors';
import albumRoutes from './routes/albums';

const app: Application = express();
const PORT = process.env.PORT || 3000;

// Middleware
app.use(cors());
app.use(express.json());

// Routes
app.use('/albums', albumRoutes);

// Health check endpoint
app.get('/', (req, res) => {
  res.json({ message: 'Album API v2 - Node.js/TypeScript' });
});

// Start server
app.listen(PORT, () => {
  console.log(`Server is running on http://localhost:${PORT}`);
});

export default app;
