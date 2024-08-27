package commands.statistics;

import app.SingletonAdmin;
import app.audio.Files.Song;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public final class GetTop5Songs implements CommandRunner {

    private static final int LIMIT = 5;

    public GetTop5Songs() {

    }


     /**
     * Gets top 5 songs.
     *
     * @param commandInput the command input
     * @return the top 5 songs
      * */
     @Override
    public ObjectNode execute(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        List<String> songs = getTop5Songs();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();
        objectNode.put("command", commandInput.getCommand());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(songs));

        return objectNode;
    }

    /**
     * Gets top 5 songs.
     *
     * @return the top 5 songs
     */
    private static ArrayList<String> getTop5Songs() {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<Song> sortedSongs = admin.getSongs();
        sortedSongs.sort(Comparator.comparingInt(Song::getLikes).reversed());
        ArrayList<String> topSongs = new ArrayList<>();
        int count = 0;
        for (Song song : sortedSongs) {
            if (count >= LIMIT) {
                break;
            }
            topSongs.add(song.getName());
            count++;
        }
        return topSongs;
    }
}
