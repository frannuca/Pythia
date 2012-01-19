package fjn.pythia.networking.messages;

/**
 * User: fran
 * Date: 10/31/11
 * Time: 2:14 PM
 */

public class CommonDataMessage {

    private byte[] payLoad;

    /**
     * this is the priority assigned to the messages. Under certain cases message are expected to contain a
     * priority level to be processed by the final server recipient
     */
    private int priority = 0;



    /**
     * Setters and getters
     */

    /**
     * gets the data wrapped in this message.
     * This data is typically a byte array containing xmls, protobuf message etc...
     * @return  the internal payload byte array
     */
    public byte[] getPayLoad(){return payLoad;}
    public void SetPayLoad(byte[] data){payLoad = data;}

    public int getPriority(){return priority;}
    public void SetPriority(int value){this.priority=value;}




}
