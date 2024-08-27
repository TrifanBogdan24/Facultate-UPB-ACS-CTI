package main.sol.playlistcmds;

import fileio.input.LibraryInput;
import fileio.input.UserInput;
import main.FunctionHeader;
import main.Playlist;
import main.cmd.InCmd;
import main.cmd.OutCmd;

public final class SwitchVisibility implements FunctionHeader {

    private SwitchVisibility() {

    }

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
        SwitchVisibility obj = new SwitchVisibility();
        obj.func(library, inputcmd, outputcmd);
    }

    /**
     * implementeaza cerinta pentru schimbarea vizibiliatii unui playlist
     * aceasta metoda updateaza o instnata de Playlist, modifcand field-ul pentru vizibilitate
     *
     * @param library primeste toata arhiva cu melodii, cantece si utilizatori
     *                (playlist-urile apartin utilizatorilor)
     * @param inputcmd primeste comanda curenta de interpretat
     * @param outputcmd modifica comanda de iesire, care va afisata in fisierele din result/
     */
    public void func(final LibraryInput library, final InCmd inputcmd,
                     final OutCmd outputcmd) {

        int playlistId = inputcmd.getPlaylistId();
        String ownerName = inputcmd.getUsername();

        UserInput user = UserInput.getUserByName(library, ownerName);

        if (playlistId > user.getPlaylists().size()) {
            outputcmd.setMessage("The specified playlist ID is too high.");
            return;
        }

        Playlist playlist = user.getPlaylists().get(playlistId - 1);

        if (playlist.getVisibility().equals("public")) {
            playlist.setVisibility("private");
            outputcmd.setMessage("Visibility status updated successfully to private.");
        } else {
            playlist.setVisibility("public");
            outputcmd.setMessage("Visibility status updated successfully to public.");
        }

    }
}
