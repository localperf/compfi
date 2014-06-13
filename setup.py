
import  sys
from    distutils.core import setup
import  py2exe
import  glob

sys.argv.append("py2exe")
sys.argv.append ("-q")

from    distutils.core import setup
import  py2exe

from    distutils.filelist import findall
import  os
import  matplotlib


opts = {
    'py2exe': { 'includes': 'matplotlib.numerix.random_array',
                'excludes': ['_gtkagg', '_tkagg']
               
              }
       }
    

matplotlibdatadir       = matplotlib.get_data_path()
matplotlibdata          = findall(matplotlibdatadir)
matplotlibdata_files    = []
for f in matplotlibdata:
    dirname = os.path.join('matplotlibdata', f[len(matplotlibdatadir)+1:])
    matplotlibdata_files.append((os.path.split(dirname)[0], [f]))



setup(
    console=['fetch.py'],
    options={
             'py2exe': {
                        'packages' : ['ystockquote', 'pandas', 'matplotlib'],
                       }
            },
    data_files=matplotlib.get_py2exe_datafiles()
)

print "finis"

"""
 'dll_excludes': ['libgdk-win32-2.0-0.dll',
                                 'libgobject-2.0-0.dll']
                                
                                
                                
                                
                                            
"""