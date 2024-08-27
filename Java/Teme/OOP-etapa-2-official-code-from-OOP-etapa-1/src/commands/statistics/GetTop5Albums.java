package commands.statistics;

import app.SingletonAdmin;
import app.audio.Collections.Album;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import commands.CommandRunner;
import fileio.input.CommandInput;

import java.util.ArrayList;
import java.util.Comparator;

public class GetTop5Albums implements CommandRunner {

    private static final Integer FIVE = 5;

    public GetTop5Albums() {

    }

    /**
     * GetTop5Albums object node
     *
     * @param commandInput      the input command from the input/test* files
     * @return                  the ouput command, on object node that will be written
     *                          in the files of the result/ directory
     */
    @Override
    public ObjectNode execute(final CommandInput commandInput) {
        ArrayList<String> result = getTop5AlbumsResult();

        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();

        objectNode.put("command", commandInput.getCommand());
        objectNode.put("timestamp", commandInput.getTimestamp());
        objectNode.put("result", objectMapper.valueToTree(result));

        return objectNode;
    }

    /**
     *
     * @return      will return an ArrayList of Strings representing artist names
     *              The result field of the output command for getTop5Albums
     */
    public static ArrayList<String> getTop5AlbumsResult() {
        SingletonAdmin admin = SingletonAdmin.getInstance();
        ArrayList<Album> albums = admin.getAlbums();

        // sorting the database albums
        albums.sort(new Comparator<Album>() {
            @Override
            public int compare(final Album album1, final Album album2) {
                int nrLikes1 = album1.getLikes();
                int nrLikes2 = album2.getLikes();

                // desc by the number of likes
                if (nrLikes1 != nrLikes2) {
                    return (nrLikes2 - nrLikes1);
                }

                // for the same number of the likes
                // asc lexicographically by the name
                String name1 = album1.getName();
                String name2 = album2.getName();
                return name1.compareTo(name2);
            }
        });

        // selecting maximum 5 albums for the ouput command
        ArrayList<String> topAlbums = new ArrayList<>();
        for (Album album: albums) {
            if (topAlbums.size() >= FIVE) {
                break;
            }

            topAlbums.add(album.getName());
        }

        return topAlbums;
    }

}
