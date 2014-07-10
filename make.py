
import  sys
from    distutils.core import setup
import  py2exe
import  glob

sys.argv.append("py2exe")
sys.argv.append ("-q")

sys.path.append("d://python_projects//compfi")

for q in sys.path:
    print "\t", q
print

from distutils.core import setup
import py2exe

from distutils.filelist import findall
import os
import matplotlib
matplotlibdatadir = matplotlib.get_data_path()
matplotlibdata = findall(matplotlibdatadir)
matplotlibdata_files = []
for f in matplotlibdata:
    dirname = os.path.join('matplotlibdata', f[len(matplotlibdatadir)+1:])
    matplotlibdata_files.append((os.path.split(dirname)[0], [f]))


opts = {
    'py2exe': { 'includes': 'matplotlib.numerix.random_array',
                'excludes': ['_gtkagg', '_tkagg'],
                'dll_excludes': ['libgdk-win32-2.0-0.dll',
                                 'libgobject-2.0-0.dll']
              }
       }


setup(
    console=['fetch.py'],
    options={
             'py2exe': {
                        'packages' : ['matplotlib', 'pandas', 'ystockquote'],
                       }
            },
    data_files=matplotlibdata_files
)