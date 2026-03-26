import express, { Request, Response } from "express";
import cors from "cors";
import albumsRouter from "./routes/albums";

const app = express();
const port = process.env.PORT ? parseInt(process.env.PORT, 10) : 3000;

app.use(cors());
app.use(express.json());

app.get("/", (_req: Request, res: Response) => {
  res.send("Welcome to the Albums API");
});

app.use("/albums", albumsRouter);

app.listen(port, () => {
  console.log(`Albums API listening on port ${port}`);
});
