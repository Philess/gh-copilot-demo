export interface Album {
  id: number;
  title: string;
  artist: string;
  price: number;
  image_url: string;
}

export type CreateAlbumDto = Omit<Album, 'id'>;
export type UpdateAlbumDto = Partial<CreateAlbumDto>;
