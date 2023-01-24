//#include <iostream>
//#include <string>
//
//int wordNest(const std::string& word, std::string nest)
//{
//	int count = 0;
//	int word_size = word.size();
//	while (nest.size() != word_size){
//		int position_word = -1;
//		int nest_size = nest.size();
//		for (int i = 0; i < nest.size(); i++)
//		{
//			if (nest.substr(i, word_size) == word)
//			{
//				position_word = nest.substr(i, word_size) == word ? i : position_word;
//				nest = nest.erase(position_word, word_size);
//				nest_size = nest.size();
//				count += 1;
//				break;
//			}
//		}
//	}
//	return count;
//}
//
////validWordNest("redeem", "rederedredrredredrerrrederedeememedeemedeemedeeemeemmedeemeemeemem"), Equals(false)); }
////validWordNest("survey", "sursursurvsurvssurssursusurveyrveyveyurveyveyurveyeyeyveyvey"), Equals(true)); }
////validWordNest("sensation", "sensatissenssensastssenensensasenssensensensationsationsationationtionsationatioionantionensationon"), Equals(false)); }
////validWordNest("feed", "feefeeded"), Equals(false)); }
////validWordNest("station", "ststatstasstatistationontationtionionation"), Equals(true)); }
////validWordNest("quarrel", "quaquarrquarrelrerell"), Equals(false)); }
////validWordNest("broadcast", "broadcbroadcastbroadcastast"), Equals(false)); }
////validWordNest("diet", "diet"), Equals(true)); }
////validWordNest("park", "pppappappapapapapparkarkarkrkrkrkrkkarkrkrarkark"), Equals(false)); }
////validWordNest("undermine", "undermiundermundermiunununderundermineminederminedermineneinene"), Equals(true)); }
////validWordNest("nail", "nannnailnailailil"), Equals(false)); }
////validWordNest("show", "sshssshowhowhowowhow"), Equals(true)); }
////validWordNest("demand", "dedemdeamademademandndndmandnd"), Equals(false)); }
////validWordNest("publicity", "publicppublicityublicityity"), Equals(true)); }
////validWordNest("relief", "rrerelirerreerrereliefliefelielifliefliefefliefelfeief"), Equals(false)); }
////validWordNest("pipe", "ppppppipeipeipeipeipeipe"), Equals(true)); }
////validWordNest("diagram", "diargdiadidiadiagramgramagramgramam"), Equals(false)); }
////validWordNest("salt", "ssaltalt"), Equals(true)); }
////validWordNest("pioneer", "pionpippipioppionpiopipioneeroneerneereerioneerneeroneerioneeroneereer"), Equals(true)); }
//
//
//int main()
//{
//	std::cout << wordNest("engagement", "engenengagemengagemeengagementntentgagementagement") << std::endl;
//
//	std::cin.get();
//}