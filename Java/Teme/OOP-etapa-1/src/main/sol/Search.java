package main.sol;


import main.FunctionHeader;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;

import fileio.input.LibraryInput;
import fileio.input.PodcastInput;
import fileio.input.SongInput;
import fileio.input.UserInput;

import main.Playlist;
import main.stats.SongStatus;
import main.stats.PodcastStatus;

import java.util.ArrayList;

public final class Search implements FunctionHeader {

    private final int five = 5;

    private Search() {

    }

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        Search obj = new Search();
        obj.func(library, inputcmd, outputcmd);
    }

    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * search
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd, final OutCmd outputcmd) {

        // orice modificare a variabilelor va fi facuta globala
        SingletonInformation info = SingletonInformation.getInstance();


        if (info.getLoadedPodcast() != null) {
            /** updatam timpul curent la podcast si la episod */
            PodcastStatus.updatePodcastTime(inputcmd.getTimestamp());
        }

        // se scoate sursa care era incarcata in player

        info.setResultedNamesForSearch(null);
        info.setResultedSearchedSongs(null);
        info.setResultedSearchedPlaylists(null);
        info.setResultedSearchedPodcasts(null);

        info.setSelectedSong(null);
        info.setSelectedPodcast(null);
        info.setSelectedPlaylist(null);

        info.setLoadedSong(null);
        info.setLoadedPodcast(null);
        info.setLoadedPlaylist(null);

        info.setLoaded(false);
        info.setResultSelect(null);



        info.unloadPlayer();



        ArrayList<String> resultedNamesForSearch = new ArrayList<String>();
        ArrayList<SongInput> resultedSearchedSongs = new ArrayList<SongInput>();
        ArrayList<Playlist> resultedSearchedPlaylists = new ArrayList<Playlist>();
        ArrayList<PodcastInput> resultedSearchedPodcasts = new ArrayList<PodcastInput>();

        int nrResults = 0;

        InCmd.Filter filtru = inputcmd.getFilters();

        if (inputcmd.getType().equalsIgnoreCase("song")) {

            // scurgem timpul din melodia redata curent

            if (!info.isPaused()) {
                SongStatus.updateSongPlayTime(inputcmd.getTimestamp());
            }

            info.setPaused(true);

            for (int i = 0; i < library.getSongs().size() && nrResults < five; i++) {

                SongInput song = library.getSongs().get(i);

                if (searchSongByFilters(song, filtru)) {
                    resultedSearchedSongs.add(song);
                    resultedNamesForSearch.add(song.getName());
                    nrResults = nrResults + 1;
                }
            }

            info.setResultedSearchedSongs(resultedSearchedSongs);

        } else if (inputcmd.getType().equalsIgnoreCase("playlist")) {


            // ne uitam in toate playlist-urile publice (ale tuturor utilizatorilor)
            for (UserInput user: library.getUsers()) {
                for (Playlist playlist : user.getPlaylists()) {

                    if (searchPlaylistByFilters(playlist, user, filtru)) {
                        resultedSearchedPlaylists.add(playlist);
                        resultedNamesForSearch.add(playlist.getName());
                        nrResults = nrResults + 1;
                    }

                    if (nrResults >= five) {
                        break;
                    }
                }
            }

            info.setResultedSearchedPlaylists(resultedSearchedPlaylists);

        } else if (inputcmd.getType().equalsIgnoreCase("podcast")) {

            for (int i = 0; i < library.getPodcasts().size() && nrResults < five; i++) {

                PodcastInput podcast = library.getPodcasts().get(i);

                if (searchPodcastByFilters(podcast, filtru)) {
                    resultedSearchedPodcasts.add(podcast);
                    resultedNamesForSearch.add(podcast.getName());
                    nrResults = nrResults + 1;
                }
            }

            info.setResultedSearchedPodcasts(resultedSearchedPodcasts);


        }

        info.setResultedNamesForSearch(resultedNamesForSearch);
        outputcmd.setMessage("Search returned " + nrResults + " results");
        outputcmd.setResults(resultedNamesForSearch);
    }

    private static boolean searchSongByFilters(final SongInput song, final InCmd.Filter filtru) {

        if (filtru.getName() != null) {
            if (!song.getName().startsWith(filtru.getName())) {
                return false;
            }
        }

        if (filtru.getAlbum() != null) {
            if (!song.getAlbum().equals(filtru.getAlbum())) {
                return false;
            }
        }

        if (filtru.getLyrics() != null) {
            if (!song.getLyrics().toLowerCase().contains(filtru.getLyrics().toLowerCase())) {
                return false;
            }
        }

        if (filtru.getTags() != null) {
            for (String filtertag : filtru.getTags()) {
                if (!song.getTags().contains(filtertag)) {
                    return false;
                }
            }
        }

        if (filtru.getGenre() != null) {
            // trebuie sa inceapa cu majuscula
            if (!song.getGenre().toLowerCase().contains(filtru.getGenre().toLowerCase())) {
                return false;
            }
        }

        if (filtru.getReleaseYear() != null) {
            String comparator = filtru.getReleaseYear();

            int cmpyear = 0;

            if (comparator.startsWith("<")) {
                // trebuie sa fie mai mica
                cmpyear = Integer.parseInt(comparator.substring(1));
                if (song.getReleaseYear() >= cmpyear) {
                    return false;
                }

            } else if (comparator.startsWith(">")) {
                // trebuie sa fie mai mare
                cmpyear = Integer.parseInt(comparator.substring(1));
                if (song.getReleaseYear() <= cmpyear) {
                    return false;
                }

            } else {
                // egala
                cmpyear = Integer.parseInt(comparator);
                if (song.getReleaseYear() != cmpyear) {
                    return false;
                }

            }

        }

        if (filtru.getArtist() != null) {
            if (!song.getArtist().equals(filtru.getArtist())) {
                return false;
            }
        }


        return true;
    }

    private static boolean searchPlaylistByFilters(final Playlist playlist, final UserInput user,
                                                   final InCmd.Filter filtru) {

        // nu ne putem uita in playlist-urile PRIVATE ale altora
        if (playlist.getVisibility().equalsIgnoreCase("private")) {
            return false;
        }

        if (filtru.getName() != null) {
            if (!playlist.getName().toLowerCase().startsWith(filtru.getName().toLowerCase())) {
                return false;
            }
        }

        if (filtru.getOwner() != null) {
            if (!playlist.getOwner().equals(filtru.getOwner())) {
                return false;
            }
        }

        return true;
    }
    private static boolean searchPodcastByFilters(final PodcastInput podcast,
                                                  final InCmd.Filter filtru) {

        if (filtru.getName() != null) {
            if (!podcast.getName().startsWith(filtru.getName())) {
                return false;
            }
        }

        if (filtru.getOwner() != null) {
            if (!podcast.getOwner().equals(filtru.getOwner())) {
                return false;
            }
        }

        return true;
    }
}
