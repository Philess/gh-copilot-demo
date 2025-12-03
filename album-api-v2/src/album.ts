export interface Album {
  id: number;
  title: string;
  artist: string;
  year: number;
}

// Sample data matching the .NET API
export const albums: Album[] = [
  { id: 1, title: "Abbey Road", artist: "The Beatles", year: 1969 },
  { id: 2, title: "The Dark Side of the Moon", artist: "Pink Floyd", year: 1973 },
  { id: 3, title: "Thriller", artist: "Michael Jackson", year: 1982 },
  { id: 4, title: "Back in Black", artist: "AC/DC", year: 1980 },
  { id: 5, title: "Rumours", artist: "Fleetwood Mac", year: 1977 }
];
