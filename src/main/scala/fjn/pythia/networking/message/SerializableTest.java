package fjn.pythia.networking.message;

import javax.xml.bind.annotation.XmlRootElement;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/31/12
 * Time: 10:53 PM
 * To change this template use File | Settings | File Templates.
 */
@XmlRootElement
public class SerializableTest {
    public SerializableTest()
    {
        personId=10;
    }
    private Integer personId;
    public void setPersonId(Integer id ){personId=id;}
    public Integer getPersonId(){return personId;}
}
