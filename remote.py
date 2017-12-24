import natlink, gramparser, xmlrpclib, socket
from SimpleXMLRPCServer import SimpleXMLRPCServer
from SimpleXMLRPCServer import SimpleXMLRPCRequestHandler
import SocketServer, threading, time

grammars = {}

server = SimpleXMLRPCServer(("", 8000))
server.timeout = 0

endpoint = xmlrpclib.ServerProxy("http://192.168.56.1:5002")

sock = socket.socket(socket.AF_INET,
                     socket.SOCK_DGRAM)

def send_result(s):
    sock.sendto(s, ("192.168.56.1", 5003))

def shutdown():
    print "shutting down"
    natlink.setTimerCallback(None)
    for g in grammars:
        grammars[g].unload()
    natlink.natDisconnect()


def guarded(f):
    def inner(*args, **kwargs):
        try:
            f(*args, **kwargs)
        except KeyboardInterrupt as e:
            print e
            shutdown()

    return inner

@guarded
def results_callback(a, b):
    print "results"
    print a
    print b.getWordInfo(0)
    print b.getWords(0)
    encoded = "\0".join(map(lambda e: e[0], a))
    print encoded
    
    try:
        send_result(encoded)
    except Exception as e:
        print e

def begin_callback(a):
    drainRequests()

def drainRequests():
    server.handle_request()

def rpc(f):
    server.register_function(f)
    return f
        
@rpc
def loadGrammar(grammar_name, spec):
    print "loading " + grammar_name
    if grammar_name in grammars:
        unloadGrammar(grammar_name)
    
    g = natlink.GramObj()
    g.setBeginCallback(begin_callback)
    g.setResultsCallback(results_callback)
    g.setHypothesisCallback(None)
    g.load(spec.data, False, False)
    grammars[grammar_name] = g
    return "done"

@rpc
def unloadGrammar(grammar_name):
    g = grammars[grammar_name]
    g.unload()
    del grammars[grammar_name]
    return "done"

#TODO:

@rpc
def setExclusive(grammar_name, exclusive):
    grammars[grammar_name].setExclusive(exclusive)
    return "done"

@rpc
def activate(grammar_name, rule_name):
    grammars[grammar_name].activate(rule_name, 0)
    return "done"

@rpc
def deactivate(grammar_name, rule_name):
    grammars[grammar_name].deactivate(rule_name)
    return "done"


#@rpc
#def updateList(grammar_name, list_name, elements): Pass


try:
    print "loading"
    natlink.natConnect(1)
    natlink.openUser("Michael")
    natlink.setMicState("on")
    natlink.setTimerCallback(drainRequests, 100)
    natlink.waitForSpeech(0)
except Exception as e:
    print e
finally:
    shutdown()

        
