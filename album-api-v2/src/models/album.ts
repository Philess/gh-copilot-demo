export interface Album {
  id: number;
  title: string;
  artist: string;
  price: number;
  image_url: string;
}

// In-memory data store with sample albums
let albums: Album[] = [
  {
    id: 1,
    title: "You, Me and an App Id",
    artist: "Daprize",
    price: 10.99,
    image_url: "https://aka.ms/albums-daprlogo"
  },
  {
    id: 2,
    title: "Seven Revision Army",
    artist: "The Blue-Green Stripes",
    price: 13.99,
    image_url: "https://aka.ms/albums-containerappslogo"
  },
  {
    id: 3,
    title: "Scale It Up",
    artist: "KEDA Club",
    price: 13.99,
    image_url: "https://aka.ms/albums-kedalogo"
  },
  {
    id: 4,
    title: "Lost in Translation",
    artist: "MegaDNS",
    price: 12.99,
    image_url: "https://aka.ms/albums-envoylogo"
  },
  {
    id: 5,
    title: "Lock Down Your Love",
    artist: "V is for VNET",
    price: 12.99,
    image_url: "https://aka.ms/albums-vnetlogo"
  },
  {
    id: 6,
    title: "Sweet Container O' Mine",
    artist: "Guns N Probeses",
    price: 14.99,
    image_url: "https://aka.ms/albums-containerappslogo"
  }
];

export function getAllAlbums(): Album[] {
  return albums;
}

export function getAlbumById(id: number): Album | null {
  return albums.find((album) => album.id === id) || null;
}

export function addAlbum(album: Omit<Album, 'id'>): Album {
  const newId = albums.length > 0 ? Math.max(...albums.map((a) => a.id)) + 1 : 1;
  const newAlbum: Album = { ...album, id: newId };
  albums.push(newAlbum);
  return newAlbum;
}

export function updateAlbum(id: number, albumData: Omit<Album, 'id'>): Album | null {
  const index = albums.findIndex((album) => album.id === id);
  if (index === -1) {
    return null;
  }
  const updatedAlbum: Album = { ...albumData, id };
  albums[index] = updatedAlbum;
  return updatedAlbum;
}

export function deleteAlbum(id: number): Album | null {
  const index = albums.findIndex((album) => album.id === id);
  if (index === -1) {
    return null;
  }
  const [deletedAlbum] = albums.splice(index, 1);
  return deletedAlbum;
}

export function resetAlbums(): void {
  albums = [
    {
      id: 1,
      title: "You, Me and an App Id",
      artist: "Daprize",
      price: 10.99,
      image_url: "https://aka.ms/albums-daprlogo"
    },
    {
      id: 2,
      title: "Seven Revision Army",
      artist: "The Blue-Green Stripes",
      price: 13.99,
      image_url: "https://aka.ms/albums-containerappslogo"
    },
    {
      id: 3,
      title: "Scale It Up",
      artist: "KEDA Club",
      price: 13.99,
      image_url: "https://aka.ms/albums-kedalogo"
    },
    {
      id: 4,
      title: "Lost in Translation",
      artist: "MegaDNS",
      price: 12.99,
      image_url: "https://aka.ms/albums-envoylogo"
    },
    {
      id: 5,
      title: "Lock Down Your Love",
      artist: "V is for VNET",
      price: 12.99,
      image_url: "https://aka.ms/albums-vnetlogo"
    },
    {
      id: 6,
      title: "Sweet Container O' Mine",
      artist: "Guns N Probeses",
      price: 14.99,
      image_url: "https://aka.ms/albums-containerappslogo"
    }
  ];
}
