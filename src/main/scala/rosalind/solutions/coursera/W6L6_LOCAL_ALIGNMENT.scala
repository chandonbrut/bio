package rosalind.solutions.coursera

import javax.swing.{JFileChooser, JComponent}
import rosalind.util.SplitLineMatrixReader
import rosalind.algo.{ScoredLocalAlignmentGraph, ScoredAlignmentGraph}

/**
 * Created by Jonas on 24/12/13.
 */

/*
Sample Input:
     MEANLY
     PENALTY

Sample Output:
     15
     EANL-Y
     ENALTY
 */
object W6L6_LOCAL_ALIGNMENT extends JComponent with App with SplitLineMatrixReader {
  val file = new JFileChooser();
  file.showOpenDialog(this)

  val scores = read(file.getSelectedFile)

  //  val v = "HIFHLYYRLNEDTKMLKGMIHVPEFWQFGMACANYNWYPFNTVQQWGYFTHLPNWCMYLDIPLWTKDYTTPRHVYGIGSLFIVGKNWRTQTFSHIADHIFGNCTTQMRAFLACMVWWVSKNHKDTLGWNPGTGMWFLMRMCDADVAVECAYCVPHICNVSNCASKKRTRTEFYHMRIRAVKWGYMIISSACWNHVGCGNDVHAASIWWVACGQNHHRPQQKTINMRWEGMTWARWYRNRDKPTHWSCARLQEKRETKPRNEMWVTTYEWHTQMLGAWRIDCGVGNNCPWCQKMGMMRQDFCHHYCSFAHRKQPMGMVWFAVQCPSLPFMQIRSGLIEAYVEVNMFHLTYEAMTRGSVDNMEHKDMNFVQWPTNCNYAEKCWQYYNQFRAYANYRHMASTCVCGYHIHVYRRSEAGVDWEVEDEGLSSVCNMSRHAQPKYVQPWMDINELSWTDMHWIPLMPAFREYCVAASRFMWHKLQQFRQEGAPIRADTVKWSAVYMIKFATFKQATLTGWTPKGTWIYRPLFYYCYVPMIWVMSRQKHQCTELEIKRWEMAHYGVDIWNRKWKEQYNNSHNSDFCEGGIYRHFGQYLAECMLHAVFQDMRAVTSMLNLIAGLNDNHYKTIYPINGSWDREALAMKTKCCQRHAYIMAYCPFQDKFMYWDRYYPGDKLMVNYPSSNNTTYTEPDVLQMMRTVEAPIRTEYKGFAKVGEETEHCIVVDVVIHINIANFRELCSLTTTSEQQDHIGTVRCFQHRIAVHCAGKFWNAFCYHFDHQHMTRCAFQACEFSNPYKTLWVTVGWYRIPWCGSNQFTWYIRNVQYCQEFIVNHTTD"
  //  val w = "SIFHHPQQTTTYYRLLYGMIHVPEFWQDGMACNTVQQWNTGYFTHLYLDIPLWTKDGIGSLVVGKNHIADHIFGNCTTQVRGFLACMVWWVSKNHKDNLVWHEKWPGTQFWFLMRMCDADVANECMYCCTEEHICNVSNCASKKRTRTEFYHMRIRAYKWGYMIISSACVGCGNDVHAASIWQVACGQNHHTPFRQSDPQHKTRIPGCYRITNMREEGMTWARWYRHWSEKRETKTYEWHTGAWFWVTKAINYDWVKMFMNATQKMGKMRQDGCHHVLDHHCSFAHRKNPMGRVVWFAVQEPCTPFEGINQQIRPGLIEAYVEVNRKHLTYELMTPGSFDNMEHKDMNFVQWPTLNSWLFIPREKCWQYYNQFRAYANPGIAHRHMASTHVYSRSEVDYEVEDEGYSSVCSKDHTQVMSRHAQPKYQGLIELSWTDMHWYPAMPAFREYCASRFMWIWFKQQFRQEGAPIRAPTVKWGAVYMINEQDVQFATFKQALYELQLTGWTPKGTWIYRSLFYYCITHLKVPMIQQYFGAVMSRFHHQCTELEIKRWEGAHYGVDIWNNHHNSDFCQGGIYRHHGQYAECMLHAVFQLNLIAGLNDNHYKTIPPINGSWDREADAMKTKCKHAYIMAYCPFQDKTRYWDRYYPGQKLMRNYPSSNNTTYTEPDVSYKGFAKNGEETEHVDHVNFRELCTTTSEKQDHAGVVRCFQFRIAVHCAGKFWNMYCYHFQHQHGTVSVSECSTRNAFQACIFSNLYNHEFYYTVTVGWYRIPLCGSNQFTWYIRNVQYCQDFIVNHTTD"

//  val v = "IHGHEHNFIDSMCCSCPVMPPKEGMDVKNFPTVVCMEYEKLHAVFRWQFKQIWFEVTRGKGWGNLIAPKGMSFDAIFSVIDFTWYMCGPVDEHHSQELDMVESLYSSASTARHTPYRAPVHVQKTGDELPEVKSYQRHWMEDQSNNHYYNRKWDGMVCGRKSWYYAEGLEHYYHCDYHGIIIYWLRRWARQFDIDNKATGTNDDSNLSKPNAAYECWVRHLQAMWVVFWMINAVCEEELNMWVLHKFHTVHCVSGQYDPTCHRIAYQRRHNCWAHYDGCWCTKPVMHCINVRFHNMQIDVKQITWNMSCWQGFFCGGKGATLNCCSYAPMCHQDQNCSPWDLQELGNPKGLKRYQVTHVVQVAKWGAIMCKVWMYLCTCMGKQKPAGWCKDCGFPAAMMLMRMPVVPLRWMWPIADDEHMVYVVSSYCFNYCYVVISRHMWTNGSMAFGKGWAGEPKQEADMGVVLIYVYDPGDISNWTQGWCKNYCSMNWERYLMNGTWPKISNKSSSVSVRAWYCWLVWGLIVAHGGKLYVWAMSEIKCGGNAATFYHGEDPKFLWVDGYYEKDKCKPTDYKLTDFHQLACSNTYGWHFGEMTCRNLWRGLQGNGWPHAISWSVNMESGWHCMQPKINESFTLIYRHNKFHTGSNWWLAVIFCMLFLGSAWWFRFCYLNKYMYQQCYCFSDWDGQWLKFIGPIYGRQHYEVDAWVHSSQWPQACCCDFGDQMMDDGMINYHPDWDCIEHGPTFQFQKDFLPKPDLHDGMASYPEWWYWQDMDHWVQLKKMHCGHDMHRAYIRPTRTSGVVPSINERRGYWGHYGWRFIVSGLHTHFDANPHWCKLMPWRWYEKLDFLSTIRIVDVEFIRGDRVNDRWFFFNYFMSTDWGYFDSFFRLGHMIEETPCWE"
//  val w = "EQWLNRYQKPKPKCCYRSYPLNDTVVMQATSWNWFAVSTWRDDMREDWGEFGAALPHFHDDYGPENCIQMCIYQGVQVLVFGHAEAWMCMCLNVCGQQYCVYYAHTEYTESMLGPMRFWKSDSCWSPWMKHAVQVQERRGHGARPDYWCASEWDYRMAPHEHQMDVGKFQYEWFCPWFLNCWHDWRFPRWMTRPTLWYFSINFLCEYFRWTDSLSTSRCELSLQSRQWCQKEIVQVEHNVVHFSARASWSRLHSKIVASPGSCSTDAMSAWGGCWYCFKMHRHCLNVRFHTMQIDLKQITWNMSCWQGFLNCCSYAPMCHNDDLFEELGNPKRYQVTHVVRVAKWLEITCMGKQKPQIPANQCDRGRCKDCGFPAAMMLFRSPVVPLRWMWPIADDEHMVYIVSSRVVHSRHMWTNGSMAFGKGKSCIVMADMGVMLIYVYNPVEAPAMRQDISNWTQGWCKNYNWERPMNGTWPKISNKSSSVSVRAWYCWLVQAFPFDHMQPSEIKCGGQDWSTEQTSQETFIPFQGPQYHFPDLWVDGYYYKMTDFHQLACSNTYVWHFGMMTCRNLWRGLMFGNGWPHAISWSVEWGMEDNDRDHTINWKCQQPKRSSKVLEYIWHKPTGKNMKFCLGGDYFDSYAQYFREWRRHMRAFEDFVWEPSDVEISVPINPGCEYTMCTGFFGDKGIDEPAADMCELVRWLSPYVVWNKHQNYLLEVQTDNTWWMDCWCFYCIRWAFKIGQWESSRMFFKCLNWEWALTEFQDPNPRNAHEGDYVYESHTWFFKICWIFKTDNFMISSRFWPINYNRWMGRQNQPCHTEMVPKVNRKCLDNQVQTNLMDAPWMVEMLETLKHFSCGLRPGNWVQRHIYHVWSAFHIRIQMWVRTQVDCASEMRPVKKRYVP"

  val v = "KGFAAMLPKIQISIREIVDELQPGSWDPINFPPQWDDAVQMIMLFFHWICHYDQQYLARLSTSETFNCCMRAIVPDNCHEQIHDFSSIIMDQLTNCMCDSQWSDNYHAGWDNDCWTIPYFIIGTEVIHLGCLIYTYNLKENLDSCVPSWSPVAMTEIDMVGELFMYCVYLIHDHNKKPNCGDQYREVNCYHYPWEIEYSDCARFGRFGLPDEWLFTRWESMSMDQGIFMIMWSQRKFIWVFMKVWGEKISIIHAWDQNNHSIASDTGMYPATIHYEWPCEAANYVQIVEVSSYMPWCCCRMRETQFSEPSHTCGQMKNNCYIVDAMWNLKIAWNSCDCYPDAVIIYILYLFHESIWPWSYMVDYVLWWRIIIEYDIPQSCHHLFLPGMCDQFFRHACDPQMWGNIQLRVINIPIGIKFEKNGMHKKWGPNKDYIHNQLYPGRKNMHTEGEELWLCMWRERSQMAMISKVFLHPELRFVNIWSSFVNNVVIRIRQSTVDYMWRVSFPFRSHFGRMKYFWVWHGWECNRLQTTFFAKTASSFYGATGWWGQYEGCDKYSKWGWENCENLTWMSWPGRMNVSLCYDDSMFNNQEICGECCQCFGIWCSVFCYIQIRHRPNYCQICVTWMWWSRTFFHVNWNCRLHCSIKKWMVNTAETVRINAEILSNRQNGTSAFWPPSFGLTIMMMPRFPRGFQLMFQAATNILMMKRHLWCFCIDAWCIRHSIKSGIVASGWWHNSVVMKDTPPVIDDCMICEIPMSHQNFTFERPTEKMECEKERTMHARLWDQLKIMGSEKIMLAGDMVVWGQQGTMYWTMVFEPSYYWTVPTACIGFNSITWILFGQWCKGAGKNSQYYDTNPAFYSERNRCVACYLAIRCNWFPFQFACRGDTTMWRYRTHPCDIN"
  val w = "YDWHVQYAGVCSGTRGMWSQCMYLNKCCYTKLVGKYDLVICWINQGWSPQQWQPNYTIHCSVVKHIYPKAHCGNIQPFCCMWKWCDCGKYDDCEEGDSAWVVREDHGRRPKIKFGRWHGAWPPENDETQPQPFLRDDFPDSDTNFKYLHWTLTPWGWFFIDYKIFKHCQYNQGSACCVMLTHAQNTILGCLFEHCDKSCMCTNERFIACDYFDLFSKKGTNRAWGWMMLIAFQYQWESHCEANSLRMQPSIIHCWQNNHSIASDTGMHPATIHYEWPCEAANYVQVEVSSYMPQLGCRCCMDTQFSEQNCDAYILALFHESIAPWSYYTYVDQYVDKVKQKVQSCHPLLLLPGFVAIKCDQFPQMWLRVINTFEKHGPNGDYIHNQLYEEITNHIPNGRKNMLTEGEEMWRERFLHPELRTFQMVNIWISFVNNVVIRIRQSTVYYMWWFTADFHSHFGRMKYFWLWHGWRLIKIAPWMMMTIFETELMCEYFSFYWWNQYEGWENCENLTWMSWPGSWIFYCGQMNVGDSLRFTPICSSQDIWRFLICGECCQCFGIWEPITRHRPIYCQICMFRHTWMWWSRTFFHVAWNCSTYILHLFWGTEKQQDDRYTAACCCHWLYNIWWAAQNIAMCNNRILTTVREERTAWTCECIWCSRCMERLPMCFVPGTKIRMSDFKQYPPPWFVNTSCQHRSTRHVYICDTYNPTHPVLCGGACKFEGNQVYWSANKYDNIYEWGTGTFPNHWQFWEGYIPQLDSYVRKKEYIYRQMDTTWKLGMLYKTFFRTWANDSQAWVLMFARDCCLRHTHDIDPYGVQVAHQKPNEANCMGDHDHAFMFVLPTPFIQRNCQQFCNFYRCCQCRNYFCYIS"

//  println(scores.mkString("\n"))

  val graph = new ScoredLocalAlignmentGraph(v,w,scores,5)
  graph.printBacktrack()
  val maxNode = graph.getMaxNode()
  graph.output(maxNode._1,maxNode._2,List(),List())


}