import { Router, Request, Response } from "express";
import { albums } from "../models/album";

const router = Router();

router.get("/", (_req: Request, res: Response) => {
  res.json(albums);
});

router.get("/:id", (req: Request, res: Response) => {
  const id = parseInt(req.params.id, 10);

  if (isNaN(id)) {
    res.status(400).json({ message: "Invalid album id" });
    return;
  }

  const album = albums.find((a) => a.id === id);

  if (!album) {
    res.status(404).json({ message: `Album with id ${id} not found` });
    return;
  }

  res.json(album);
});

export default router;
