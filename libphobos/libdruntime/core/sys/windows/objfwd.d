/**
 * Windows API header module
 *
 * Translated from MinGW Windows headers
 *
 * License: $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source: $(DRUNTIMESRC src/core/sys/windows/_objfwd.d)
 */
module core.sys.windows.objfwd;
version (Windows):
@system:

import core.sys.windows.objidl;

/+
// Forward declararions are not necessary in D.
extern(Windows) {
    interface IMoniker;
    interface IStream;
    interface IMarshal;
    interface IMalloc;
    interface IMallocSpy;
    interface IMessageFilter;
    interface IPersist;
    interface IPersistStream;
    interface IRunningObjectTable;
    interface IBindCtx;
    interface IAdviseSink;
    interface IAdviseSink2;
    interface IDataObject;
    interface IDataAdviseHolder;

    interface IEnumMoniker;
    interface IEnumFORMATETC;
    interface IEnumSTATDATA;
    interface IEnumSTATSTG;
    interface IEnumSTATPROPSTG;
    interface IEnumString;
    interface IEnumUnknown;
    interface IStorage;
    interface IPersistStorage;
    interface ILockBytes;
    interface IStdMarshalInfo;
    interface IExternalConnection;
    interface IRunnableObject;
    interface IROTData;
    interface IPersistFile;
    interface IRootStorage;
    interface IPropertyStorage;
    interface IEnumSTATPROPSETSTG;
    interface IPropertySetStorage;
    interface IClientSecurity;
    interface IServerSecurity;
    interface IClassActivator;
    interface IFillLockBytes;
    interface IProgressNotify;
    interface ILayoutStorage;
    interface IRpcProxyBuffer;
    interface IRpcChannelBuffer;
    interface IRpcStubBuffer;
}
+/
alias IMoniker LPMONIKER;
alias IStream LPSTREAM;
alias IMarshal LPMARSHAL;
alias IMalloc LPMALLOC;
alias IMallocSpy LPMALLOCSPY;
alias IMessageFilter LPMESSAGEFILTER;
alias IPersist LPPERSIST;
alias IPersistStream LPPERSISTSTREAM;
alias IRunningObjectTable LPRUNNINGOBJECTTABLE;
alias IBindCtx LPBINDCTX, LPBC;
alias IAdviseSink LPADVISESINK;
alias IAdviseSink2 LPADVISESINK2;
alias IDataObject LPDATAOBJECT;
alias IDataAdviseHolder LPDATAADVISEHOLDER;
alias IEnumMoniker LPENUMMONIKER;
alias IEnumFORMATETC LPENUMFORMATETC;
alias IEnumSTATDATA LPENUMSTATDATA;
alias IEnumSTATSTG LPENUMSTATSTG;
alias IEnumSTATPROPSTG LPENUMSTATPROPSTG;
alias IEnumString LPENUMSTRING;
alias IEnumUnknown LPENUMUNKNOWN;
alias IStorage LPSTORAGE;
alias IPersistStorage LPPERSISTSTORAGE;
alias ILockBytes LPLOCKBYTES;
alias IStdMarshalInfo LPSTDMARSHALINFO;
alias IExternalConnection LPEXTERNALCONNECTION;
alias IRunnableObject LPRUNNABLEOBJECT;
alias IROTData LPROTDATA;
alias IPersistFile LPPERSISTFILE;
alias IRootStorage LPROOTSTORAGE;
alias IRpcChannelBuffer LPRPCCHANNELBUFFER;
alias IRpcProxyBuffer LPRPCPROXYBUFFER;
alias IRpcStubBuffer LPRPCSTUBBUFFER;
alias IPropertyStorage LPPROPERTYSTORAGE;
alias IEnumSTATPROPSETSTG LPENUMSTATPROPSETSTG;
alias IPropertySetStorage LPPROPERTYSETSTORAGE;
alias IClientSecurity LPCLIENTSECURITY;
alias IServerSecurity LPSERVERSECURITY;
alias IClassActivator LPCLASSACTIVATOR;
alias IFillLockBytes LPFILLLOCKBYTES;
alias IProgressNotify LPPROGRESSNOTIFY;
alias ILayoutStorage LPLAYOUTSTORAGE;
