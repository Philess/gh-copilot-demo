export interface Artist {
  Name: string;
  Birthdate: string; // ISO date string
  BirthPlace: string;
}

export interface Album {
  Id: number;
  Title: string;
  Artist: Artist;
  Year: number;
  Price: number;
  Image_url: string;
}
