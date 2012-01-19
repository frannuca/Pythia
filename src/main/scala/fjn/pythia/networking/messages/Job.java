package fjn.pythia.networking.messages;

import scala.collection.mutable.ListBuffer;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 10/31/11
 * Time: 4:17 PM
 * To change this template use File | Settings | File Templates.
 */
public class Job {
    enum STATUS {WAITING,RUNNING,FINISHED,ERROR}

    public STATUS status;

    public String input;
    public String output;

    public String info;

    public Date sent;
    public Date finished;
    public Date started;

}


