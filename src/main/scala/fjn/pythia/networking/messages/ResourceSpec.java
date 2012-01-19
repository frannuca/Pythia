package fjn.pythia.networking.messages;


import java.util.Date;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/31/11
 * Time: 4:08 PM
 * To change this template use File | Settings | File Templates.
 */
public class ResourceSpec {

    public enum STATUS {ACTIVE,STOPPED,FAILURE}

    public String libraryVersion;
    public String address;
    public long port;
    public int maxNumberOfJobsInQueue = 9999;
    public String info;
    public Date started;
    public Date stopped;
    public STATUS status;
}
