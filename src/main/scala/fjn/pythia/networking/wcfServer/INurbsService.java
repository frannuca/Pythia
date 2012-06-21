package fjn.pythia.networking.wcfServer;

import fjn.pythia.networking.message.bindings.DMatrix;
import fjn.pythia.networking.message.bindings.DPoint2D;
import fjn.pythia.networking.message.bindings.DVector;

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
    @WebMethod public String InitializeInterpolator2D(@WebParam(name="x")DVector x,@WebParam(name="y")DVector y,@WebParam(name="z")DMatrix z,@WebParam(name="orderX")Integer orderX,@WebParam(name="orderY") Integer orderY);
    @WebMethod public String InitializeInterpolator1D(@WebParam(name="x")DVector x,@WebParam(name="z")DVector z,@WebParam(name="order")Integer order);
    @WebMethod public Double Compute2D(@WebParam(name="session")String sessionId,@WebParam(name="x")Double x,@WebParam(name="y")Double y);
    @WebMethod public DVector Compute2DBatch(@WebParam(name="session")String sessionId,@WebParam(name="points")DPoint2D[] points);
    @WebMethod public Double Compute1D(@WebParam(name="session")String sessionId,@WebParam(name="x")Double x);
    @WebMethod public DVector Compute1DBatch(@WebParam(name="session")String sessionId,@WebParam(name="points")Double[] points);
    @WebMethod public int DropSession(@WebParam(name="session")String sessionID);
}
