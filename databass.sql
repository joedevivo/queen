CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE albums (
    id UUID PRIMARY KEY DEFAULT uuid_generate_v4(),
    name TEXT,
    year integer
);

INSERT INTO albums (name, year) VALUES ('Queen', 1973);
INSERT INTO albums (name, year) VALUES ('Queen II', 1974);
INSERT INTO albums (name, year) VALUES ('Sheer Heart Attack', 1974);
INSERT INTO albums (name, year) VALUES ('A Night at the Opera', 1975);
INSERT INTO albums (name, year) VALUES ('A Day at the Races', 1976);
INSERT INTO albums (name, year) VALUES ('News of the World', 1977);
INSERT INTO albums (name, year) VALUES ('Jazz', 1978);
INSERT INTO albums (name, year) VALUES ('The Game', 1980);
INSERT INTO albums (name, year) VALUES ('Flash Gordon', 1980);
INSERT INTO albums (name, year) VALUES ('Hot Space', 1982);
INSERT INTO albums (name, year) VALUES ('The Works', 1984);
INSERT INTO albums (name, year) VALUES ('A Kind of Magic', 1986);
INSERT INTO albums (name, year) VALUES ('The Miracle', 1989);
INSERT INTO albums (name, year) VALUES ('Innuendo', 1991);
INSERT INTO albums (name, year) VALUES ('Made in Heaven', 1995);

