import Data.List.Split	
import Data.List

data Capability = Capability { name :: String
							 , value :: String
							 }deriving (Show)

data Group = Group { idG :: String
				   , capabilities :: [Capability]
					}deriving (Show)
					
data Device = Device { idD :: String
                     , user_agent :: String
					 , fall_back :: String
					 , groups ::[Group]
					 }deriving (Show)						 
					 
data EnumXML = Cap | DevBegin | GrBegin | DevEnd| GrEnd | DevsBegin | DevsEnd | None deriving ( Eq, Show)

crearCapability:: String -> Capability
crearCapability cadena = Capability (cad!!1) (cad!!3)
				where cad = splitOn ['"'] cadena
				
crearGroup:: String  -> Group
crearGroup cadena  = Group (cad!!1) []
				where cad = splitOn ['"'] cadena
				
crearDevice:: String -> Device
crearDevice cadena = Device (cad!!1) (cad!!3) (cad!!5) []
				where cad = splitOn ['"'] cadena

reconocerString:: String -> EnumXML
reconocerString cadena
			| "/devices" == cadena = DevsEnd
			| "devices" == cadena = DevsBegin
			| "/group" == cadena = GrEnd
			| "/device" == cadena = DevEnd
			| "capability name" `isInfixOf` cadena = Cap 
			| "group id" `isInfixOf` cadena = GrBegin
			| "device id" `isInfixOf` cadena = DevBegin	
			| otherwise = None

{- Esta funcion se encarga de convertir la lista de strings con la informacion de un solo grupo en una lista de capabilities -} 
crearListaCapabilities :: [String] -> [Capability]
crearListaCapabilities x = [ crearCapability y | y<-x, reconocerString y == Cap ]

{- Esta funcion se encarga de convertir una lista de strings en un Group, crea el grupo -} 
crearGrupoString :: [String] -> [Group]
crearGrupoString [] = []
crearGrupoString (x:xs) = [agregarCapabilitiesaGroup (crearGroup x)  (crearListaCapabilities xs)]

{- Esta funcion se encarga de convertir la lista de strings con la informacion de un solo device en una lista de groups -} 
crearListaGroups :: [String] -> [Group]
crearListaGroups []  = []
crearListaGroups listaString  = (crearGrupoString grupo) ++ (crearListaGroups resto)
		where (grupo, resto)=cogerGroupString listaString

{- Esta funcion se encarga de convertir una lista de strings en un Group, crea el grupo -} 
crearDeviceString :: [String] -> [Device]
crearDeviceString [] = []
crearDeviceString (x:xs) = [agregarGroupsaDevice (crearDevice x)  (crearListaGroups xs)]

{------------------------------------------------------------------------------------------------------------------------------------------------------------------
ESTA ES LA FUNCION PRINCIPAL QUE DEBEMOS LLAMAR PARA CREAR LA LISTA DE DEVICES DE TODO EL DOCUMENTO-}
crearListaDevices :: [String] -> [Device]
crearListaDevices []  = []
crearListaDevices listaString  = (crearDeviceString device) ++ (crearListaDevices resto)
		where (device, resto)=cogerDeviceString listaString
{---------------------------------------------------------------------------------------------------------------------------------------------------------------}



cogerGroupString :: [String] -> ( [String] , [String] )
cogerGroupString [] = ([],[])
cogerGroupString (x:xs) 
			| reconocerString x == GrBegin = cogerGroupString' (x:xs) [] 
			| otherwise = cogerGroupString xs

cogerGroupString' :: [String] -> [String] ->  ( [String] , [String] )
cogerGroupString' [] listaGrupo  = ( listaGrupo , [])
cogerGroupString' (x:xs) listaGrupo
					| reconocerString x == GrEnd = ( (listaGrupo ++ [x]) , xs)
					| otherwise = cogerGroupString' xs (listaGrupo ++ [x])

{- Esta funcion parte la lista y retorna el primer device y el resto de la cadena en una dupla -} 
cogerDeviceString :: [String] -> ( [String] , [String] )
cogerDeviceString [] = ([],[])
cogerDeviceString (x:xs) 
			| reconocerString x == DevBegin = cogerDeviceString' (x:xs) [] 
			| otherwise = cogerDeviceString xs

cogerDeviceString' :: [String] -> [String] ->  ( [String] , [String] )
cogerDeviceString' [] listaDevice  = ( listaDevice , [])
cogerDeviceString' (x:xs) listaDevice
					| reconocerString x == DevEnd = ( (listaDevice ++ [x]) , xs)
					| otherwise = cogerDeviceString' xs (listaDevice ++ [x])							
					
{- Esta funcion se encarga de agregar la lista de capalities al group correespondiente -} 
agregarCapabilitiesaGroup :: Group -> [Capability] -> Group
agregarCapabilitiesaGroup (Group id caps) cap =  Group id (caps ++ cap)

{- Esta funcion se encarga de agregar la lista de group al device correespondiente -} 		
agregarGroupsaDevice :: Device -> [Group] -> Device
agregarGroupsaDevice (Device id us fa grs) gr =  Device id us fa(grs++gr)				

{- Esta funcion convierte un string en una lista. Los elementos de la lista se crean cuando encuentra comillas ("), enter, tabs entre otros -}  								 
palabra                   :: String -> [String]
palabra s                 =  case dropWhile {-partain:Char.-}esEspacio s of
                                "" -> []
                                s' -> w : palabra s''
                                      where (w, s'') = break {-partain:Char.-}esEspacio s'

esEspacio :: Char -> Bool											 
esEspacio c             =  c == '<'     ||
                           c == '>'
						   
{--------------------------------------------------------------------------------------------------------------------------------------------------------------}
{- FUNCIONES DE BUSQUEDA -}

reconocerCapability:: Capability -> String -> [Capability]
reconocerCapability (Capability name value) cadena
				| ( name==cadena || value==cadena ) = [Capability name value]
			    | otherwise =[]

buscarCapability:: [Capability] -> String -> Bool
buscarCapability [] cadena = False
buscarCapability (cap1:restCap) cadena 
			| reconocerCapability cap1 cadena = True
			| otherwise = buscarCapability restCap cadena