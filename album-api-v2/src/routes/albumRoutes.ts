import { Router } from 'express';
import { albumController } from '../controllers/albumController';

const router = Router();

// GET /albums - Get all albums
router.get('/', (req, res) => albumController.getAll(req, res));

// GET /albums/:id - Get album by ID
router.get('/:id', (req, res) => albumController.getById(req, res));

// POST /albums - Create a new album
router.post('/', (req, res) => albumController.create(req, res));

// PUT /albums/:id - Update an album
router.put('/:id', (req, res) => albumController.update(req, res));

// DELETE /albums/:id - Delete an album
router.delete('/:id', (req, res) => albumController.delete(req, res));

export default router;
