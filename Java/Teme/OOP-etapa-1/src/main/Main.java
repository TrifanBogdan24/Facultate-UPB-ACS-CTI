package main;

import checker.Checker;
import checker.CheckerConstants;

import main.cmd.InCmd;
import main.cmd.OutCmd;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectWriter;
// import com.fasterxml.jackson.databind.node.ArrayNode;
import fileio.input.LibraryInput;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Objects;

import main.sol.podcastcmds.Backward;
import main.sol.podcastcmds.Forward;
import main.stats.Status;

import main.sol.GetTop5Songs;
import main.sol.Like;
import main.sol.Load;
import main.sol.Next;
import main.sol.Prev;
import main.sol.PlayPause;
import main.sol.Select;
import main.sol.Shuffle;
import main.sol.ShowPreferredSongs;
import main.sol.Repeat;
import main.sol.Search;

// comenzi doar pentru playlist-uri
import main.sol.playlistcmds.AddRemoveInPlaylist;
import main.sol.playlistcmds.CreatePlaylist;
import main.sol.playlistcmds.Follow;
import main.sol.playlistcmds.GetTop5Playlists;
import main.sol.playlistcmds.ShowPlaylists;
import main.sol.playlistcmds.SwitchVisibility;



/**
 * The entry point to this homework. It runs the checker that tests your implentation.
 */
public final class Main {
    static final String LIBRARY_PATH = CheckerConstants.TESTS_PATH + "library/library.json";

    /**
     * for coding style
     */
    private Main() {
    }

    /**
     * DO NOT MODIFY MAIN METHOD
     * Call the checker
     * @param args from command line
     * @throws IOException in case of exceptions to reading / writing
     */
    public static void main(final String[] args) throws IOException {
        File directory = new File(CheckerConstants.TESTS_PATH);
        Path path = Paths.get(CheckerConstants.RESULT_PATH);

        if (Files.exists(path)) {
            File resultFile = new File(String.valueOf(path));
            for (File file : Objects.requireNonNull(resultFile.listFiles())) {
                file.delete();
            }
            resultFile.delete();
        }
        Files.createDirectories(path);

        for (File file : Objects.requireNonNull(directory.listFiles())) {
            if (file.getName().startsWith("library")) {
                continue;
            }

            String filepath = CheckerConstants.OUT_PATH + file.getName();
            File out = new File(filepath);
            boolean isCreated = out.createNewFile();
            if (isCreated) {
                action(file.getName(), filepath);
            }
        }

        Checker.calculateScore();
    }

    /**
     * @param filePathInput for input file
     * @param filePathOutput for output file
     * @throws IOException in case of exceptions to reading / writing
     */
    public static void action(final String filePathInput,
                              final String filePathOutput) throws IOException {
        ObjectMapper objectMapper = new ObjectMapper();
        LibraryInput library = objectMapper.readValue(new File(LIBRARY_PATH), LibraryInput.class);

        /**
        * melodiile se acceseaza cu: library.getSongs()
        * utilizatorii se acceseaza cu : library.getUsers()
        * podcasturile se acceseaza cu : library.getPodcasts()
         */

        ObjectMapper om = new ObjectMapper();
        InCmd[] incmds = om.readValue(new File("input/" + filePathInput), InCmd[].class);


        SingletonInformation info = SingletonInformation.getInstance();
        info.initFields();

        OutCmd[] outcmds = new OutCmd[incmds.length];

        for (int i = 0; i < incmds.length; i++) {

            outcmds[i] = new OutCmd();
            outcmds[i].setDefaultFields(incmds[i]);

            switch (incmds[i].getCommand()) {
                case "search":
                    Search.solve(library, incmds[i], outcmds[i]);
                    break;

                case "select":
                    Select.solve(library, incmds[i], outcmds[i]);
                    break;

                case "load":
                    Load.solve(library, incmds[i], outcmds[i]);
                    break;

                case "playPause":
                    PlayPause.solve(library, incmds[i], outcmds[i]);
                    break;

                case "createPlaylist":
                    CreatePlaylist.solve(library, incmds[i], outcmds[i]);
                    break;

                case "switchVisibility":
                    SwitchVisibility.solve(library, incmds[i], outcmds[i]);
                    break;

                case "follow":
                    Follow.solve(library, incmds[i], outcmds[i]);
                    break;

                case "like":
                    Like.solve(library, incmds[i], outcmds[i]);
                    break;

                case "addRemoveInPlaylist":
                    AddRemoveInPlaylist.solve(library, incmds[i], outcmds[i]);
                    break;

                case "shuffle":
                    Shuffle.solve(library, incmds[i], outcmds[i]);
                    break;

                case "repeat":
                    Repeat.solve(library, incmds[i], outcmds[i]);
                    break;

                case "showPreferredSongs":
                    ShowPreferredSongs.solve(library, incmds[i], outcmds[i]);
                    break;

                case "showPlaylists":
                    ShowPlaylists.solve(library, incmds[i], outcmds[i]);
                    break;

                case "status":
                    Status.solve(library, incmds[i], outcmds[i]);
                    break;

                case "getTop5Songs":
                    GetTop5Songs.solve(library, incmds[i], outcmds[i]);
                    break;

                case "getTop5Playlists":
                    GetTop5Playlists.solve(library, incmds[i], outcmds[i]);
                    break;

                case "forward":
                    Forward.solve(library, incmds[i], outcmds[i]);
                    break;

                case "backward":
                    Backward.solve(library, incmds[i], outcmds[i]);
                    break;

                case "prev":
                    Prev.solve(library, incmds[i], outcmds[i]);
                    break;

                case "next":
                    Next.solve(library, incmds[i], outcmds[i]);
                    break;

                default:
                    break;
            }

        }

        ObjectWriter objectWriter = objectMapper.writerWithDefaultPrettyPrinter();
        objectWriter.writeValue(new File(filePathOutput), outcmds);
    }
}
