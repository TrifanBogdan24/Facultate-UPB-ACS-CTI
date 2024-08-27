package main.sol;


import fileio.input.PodcastInput;
import fileio.input.SongInput;
import main.FunctionHeader;
import main.Playlist;
import main.SingletonInformation;
import main.cmd.InCmd;
import main.cmd.OutCmd;
import fileio.input.LibraryInput;


public class Select implements FunctionHeader {

    /**
     * functie 'helper' prin intermediul careia se apeleaza
     * functia implementata din interfata 'FunctionHeader'
     * fara a mai fi nevoie de se crea o instanta a clasei
     * (lucru realizat deja in interiorul functiei)
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     * playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public static void solve(final LibraryInput library, final InCmd inputcmd,
                             final OutCmd outputcmd) {
        Select obj = new Select();
        obj.func(library, inputcmd, outputcmd);
    }


    /**
     * functie implementata din interfata 'FunctionHeader'
     * are ca scop implementarea si executarea comenzii
     *
     * select
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                playlist-urile apartin utilizatorilor
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd, final OutCmd outputcmd) {

        SingletonInformation info = SingletonInformation.getInstance();

        if (info.getResultedNamesForSearch() == null) {
            outputcmd.setMessage("Please conduct a search before making a selection.");
            return;
        }

        if (info.getResultedNamesForSearch().isEmpty()) {
            outputcmd.setMessage("The selected ID is too high.");
            return;
        }

        int idx = inputcmd.getItemNumber();

        if (idx > info.getResultedNamesForSearch().size()) {
            outputcmd.setMessage("The selected ID is too high.");
            return;
        }

        info.setResultSelect(null);
        info.setSelectedSong(null);
        info.setSelectedPodcast(null);
        info.setSelectedPlaylist(null);

        if (info.getResultedSearchedSongs() != null) {
            SongInput song = info.getResultedSearchedSongs().get(idx - 1);
            info.setSelectedSong(song);
            info.setResultSelect(song.getName());

        } else if (info.getResultedSearchedPlaylists() != null) {
            Playlist playlist = info.getResultedSearchedPlaylists().get(idx - 1);
            info.setSelectedPlaylist(playlist);
            info.setResultSelect(playlist.getName());

        } else if (info.getResultedSearchedPodcasts() != null) {
            PodcastInput podcast = info.getResultedSearchedPodcasts().get(idx - 1);
            info.setSelectedPodcast(podcast);
            info.setResultSelect(podcast.getName());
        }

        outputcmd.setMessage("Successfully selected " +  info.getResultSelect() + ".");
    }
}
