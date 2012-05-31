package fjn.pythia.networking.wcfServer;

import java.lang.reflect.Method;

/**
 * Created by IntelliJ IDEA.
 * User: fran
 * Date: 5/31/12
 * Time: 8:45 PM
 * To change this template use File | Settings | File Templates.
 */
public  class DynamicCast {
    /**
         * Convert the given object value to the given class.
         * @param from The object value to be converted.
         * @param to The type class which the given object should be converted to.
         * @return The converted object value.
         * @throws NullPointerException If 'to' is null.
         * @throws UnsupportedOperationException If no suitable converter can be found.
         * @throws RuntimeException If conversion failed somehow. This can be caused by at least
         * an ExceptionInInitializerError, IllegalAccessException or InvocationTargetException.
         */
        public static <T extends Object> T convert(Object from, Class<T> to) {

            // Null is just null.
            if (from == null) {
                return null;
            }

            // Can we cast? Then just do it.
            if (to.isAssignableFrom(from.getClass())) {
                return to.cast(from);
            }

            return  null;
        }
}

