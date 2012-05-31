package fjn.pythia.networking.wcfServer;

import fjn.pythia.networking.message.SerializableTest;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;
import javax.jws.soap.SOAPBinding;
import javax.jws.soap.SOAPBinding.Style;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/29/12
 * Time: 6:18 PM
 * To change this template use File | Settings | File Templates.
 */
@WebService
@SOAPBinding(style = Style.DOCUMENT) // more on this later
public interface INurbsService {
    @WebMethod(operationName = "InitializeInterpolator2D",action = "action2D") public String InitializeInterpolator2D(@WebParam(name="x")Double[] x,@WebParam(name="y")Double[] y,@WebParam(name="z")Double[][] z,@WebParam(name="order")Double[] order);
    @WebMethod(operationName = "InitializeInterpolator1D",action = "action1D") public String InitializeInterpolator1D(@WebParam(name="x")Double[] x,@WebParam(name="z")Double[] z,@WebParam(name="order")Double order);
    @WebMethod public Double Compute2D(@WebParam(name="session")String sessionId,@WebParam(name="x")Double x,@WebParam(name="y")Double y);
    @WebMethod public Double Compute1D(@WebParam(name="session")String sessionId,@WebParam(name="x")Double x);
    @WebMethod public int DropSession(@WebParam(name="session")String sessionID);
    @WebMethod public SerializableTest testPersonId(SerializableTest id);
}
