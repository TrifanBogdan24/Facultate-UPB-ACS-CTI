package commands.statistics;

import app.SingletonAdmin;
import app.audio.Playlist;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;
import java.util.Comparator;

public final class GetTop5Playlists implements CommandRunner {

    private static final int LIMIT = 5;


    public GetTop5Playlists() {

    }

    /**
     * Gets top 5 playlists.
     *
     * @param commandInput the command input
     * @return the top 5 playlists
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        ArrayList<String> playlists = getTop5Playlists();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(playlists));

        return objectNode;
    }


    /**
     * Gets top 5 playlists.
     *
     * @return the top 5 playlists
     */
    public static ArrayList<String> getTop5Playlists() {
        SingletonAdmin admin = SingletonAdmin.getInstance();

        ArrayList<Playlist> sortedPlaylists = new ArrayList<>(admin.getPlaylists());
        sortedPlaylists.sort(Comparator.comparingInt(Playlist::getFollowers)
                .reversed()
                .thenComparing(Playlist::getTimestamp, Comparator.naturalOrder()));
        ArrayList<String> topPlaylists = new ArrayList<>();
        int count = 0;
        for (Playlist playlist : sortedPlaylists) {
            if (count >= LIMIT) {
                break;
            }
            topPlaylists.add(playlist.getName());
            count++;
        }
        return topPlaylists;
    }
}
