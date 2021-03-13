
SPEC_BUILDER=./_build/default/llvmspec/specBuilder.exe
if [ $# -eq 0 ]
  then
    echo "no command specified"
    exit 1
fi

echo $1

if [ $1 = "create" ]
  then
    if [ $# -eq 2 ]
      then
        if [ -d $2 ]
        then
          echo "project directory already exists at $2"
        else
          echo "Creating project directory at $2"
          mkdir $2
          cp templates/Makefile $2
          echo "Creating output directory: $2/output"
          mkdir $2/output
          echo "Creating output directory: $2/output"
          mkdir $2/spec
        fi
    else
      echo "no project name specified"
    fi
fi

if [ $1 = "parse" ]; then
    if [ $# -eq 3 ]; then
        $SPEC_BUILDER $3 $2
    else
      echo "no project and bc file provided"
    fi
fi
