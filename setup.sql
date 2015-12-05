CREATE ROLE "haskellbr-website";
CREATE DATABASE "haskellbr-website";
GRANT ALL PRIVILEGES ON DATABASE "haskellbr-website" TO "haskellbr-website";
ALTER ROLE "haskellbr-website" WITH LOGIN;
ALTER ROLE "haskellbr-website" WITH PASSWORD 'haskellbr-website';
